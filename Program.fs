namespace McpDuckduckgoFs

open System
open System.ComponentModel
open System.Net.Http
open System.Text.RegularExpressions
open System.Threading.Tasks
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open ModelContextProtocol.Server

type SearchResult = {
    Title: string
    Link: string
    Snippet: string
    Position: int
}

type RateLimiter(requestsPerMinute: int) =
    let requests = System.Collections.Generic.List<DateTime>()
    member _.AcquireAsync() = task {
        let now = DateTime.Now
        requests.RemoveAll(fun t -> now - t > TimeSpan.FromMinutes(1.0)) |> ignore
        if requests.Count >= requestsPerMinute then
            let waitSeconds = 60.0 - (now - requests[0]).TotalSeconds
            if waitSeconds > 0.0 then
                do! Task.Delay(TimeSpan.FromSeconds(waitSeconds))
        requests.Add(now)
    }

[<McpServerToolType>]
type DuckduckgoTool() =
    let httpClient = new HttpClient()
    let searchLimiter = new RateLimiter(30)
    let fetchLimiter = new RateLimiter(20)

    do
        httpClient.DefaultRequestHeaders.Add(
            "User-Agent",
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"
        )

    member private _.CleanUrl(url: string) =
        if url.StartsWith("//duckduckgo.com/l/?uddg=") then
            let idx = url.IndexOf("uddg=")
            if idx >= 0 then
                let mutable cleanUrl = url.Substring(idx + 5)
                let ampIdx = cleanUrl.IndexOf('&')
                if ampIdx >= 0 then
                    cleanUrl <- cleanUrl.Substring(0, ampIdx)
                Uri.UnescapeDataString(cleanUrl)
            else url
        else url

    member private this.SearchDuckDuckGo(query: string, maxResults: int) = task {
        let results = System.Collections.Generic.List<SearchResult>()
        try
            use form = new FormUrlEncodedContent(
                [| KeyValuePair("q", query)
                   KeyValuePair("b", "")
                   KeyValuePair("kl", "") |]
            )
            let! resp = httpClient.PostAsync("https://html.duckduckgo.com/html", form)
            resp.EnsureSuccessStatusCode()
            let! html = resp.Content.ReadAsStringAsync()

            let titlePattern = """<h2 class=""result__title"">.*?<a[^>]*href=""([^"]*)""[^>]*>([^<]*)</a>"""
            let snippetPattern = """<a class=""result__snippet""[^>]*href=""[^"]*"">(.*?)</a>"""
            let titleMatches = Regex.Matches(html, titlePattern, RegexOptions.Singleline ||| RegexOptions.IgnoreCase)
            let snippetMatches = Regex.Matches(html, snippetPattern, RegexOptions.Singleline ||| RegexOptions.IgnoreCase)

            let mutable pos = 1
            for i in 0 .. min (titleMatches.Count - 1) (maxResults - 1) do
                let t = titleMatches[i]
                let link = this.CleanUrl(t.Groups[1].Value)
                let title = Regex.Replace(t.Groups[2].Value, @"<[^>]+>", "").Trim()

                let snippet =
                    if i < snippetMatches.Count then
                        Regex.Replace(snippetMatches[i].Groups[1].Value, @"<[^>]+>", "").Trim()
                    else
                        String.Empty

                if not (String.IsNullOrWhiteSpace(title)) &&
                   not (String.IsNullOrWhiteSpace(link)) &&
                   not (link.Contains("y.js")) &&
                   not (link.Contains("duckduckgo.com")) then
                    results.Add({ Title = title; Link = link; Snippet = snippet; Position = pos })
                    pos <- pos + 1

            return results |> Seq.toList
        with ex ->
            return raise (Exception($"Search request failed: {ex.Message}", ex))
    }

    member private _.FormatResultsForLLM(results: SearchResult list) =
        if results.IsEmpty then
            "No results were found for your search query. This could be due to DuckDuckGo's bot detection or the query returned no matches. Please try rephrasing your search or try again in a few minutes."
        else
            let lines = System.Collections.Generic.List<string>()
            lines.Add($"Found {results.Length} search results:\n")
            for r in results do
                lines.Add($"{r.Position}. {r.Title}")
                lines.Add($"   URL: {r.Link}")
                lines.Add($"   Summary: {r.Snippet}")
                lines.Add("")
            String.Join("\n", lines)

    member private _.FetchAndParseContent(url: string) = task {
        try
            let! resp = httpClient.GetAsync(url)
            resp.EnsureSuccessStatusCode()
            let! html = resp.Content.ReadAsStringAsync()

            let html' =
                Regex.Replace(
                    html,
                    @"<(script|style|nav|header|footer)[^>]*>.*?</\1>",
                    "",
                    RegexOptions.IgnoreCase ||| RegexOptions.Singleline
                )
            let text = Regex.Replace(html', "<[^>]+>", " ") |> fun s -> Regex.Replace(s, @"\s+", " ").Trim()
            let text = if text.Length > 8000 then text.Substring(0, 8000) + "... [content truncated]" else text
            return text
        with
        | :? HttpRequestException as ex -> return $"Error: Could not access the webpage ({ex.Message})"
        | :? TaskCanceledException -> return "Error: The request timed out while trying to fetch the webpage."
        | ex -> return $"Error: An unexpected error occurred while fetching the webpage ({ex.Message})"
    }

    interface IDisposable with
        member _.Dispose() = httpClient.Dispose()

    [<McpServerTool; Description("Search DuckDuckGo and return formatted results. This tool performs web searches using DuckDuckGo's HTML interface and returns formatted search results including titles, URLs, and snippets. Use this when you need to find current information on the web.")>]
    member this.Search([<Description("The search query string")>] query: string, [<Description("Maximum number of results to return (default: 10)")>] ?maxResults: int) = task {
        try
            do! searchLimiter.AcquireAsync()
            let maxResults = defaultArg maxResults 10
            let! results = this.SearchDuckDuckGo(query, maxResults)
            return this.FormatResultsForLLM(results)
        with ex ->
            return $"An error occurred while searching: {ex.Message}"
    }

    [<McpServerTool; Description("Fetch and parse content from a webpage URL. This tool retrieves the text content from a webpage, removes HTML markup, and returns clean text suitable for analysis. Useful for getting detailed information from specific web pages.")>]
    member this.FetchContent([<Description("The webpage URL to fetch content from")>] url: string) = task {
        try
            do! fetchLimiter.AcquireAsync()
            return! this.FetchAndParseContent(url)
        with ex ->
            return $"An error occurred while fetching content: {ex.Message}"
    }

module Program =
    [<EntryPoint>]
    let main argv =
        let builder = Host.CreateApplicationBuilder(argv)
        builder.Logging.AddConsole(fun o -> o.LogToStandardErrorThreshold <- LogLevel.Trace) |> ignore

        builder.Services
            .AddMcpServer(fun options ->
                options.ServerInfo <- ModelContextProtocol.Protocol.Implementation(Name = "mcpDuckduckgo", Version = "1.0.0")
            )
            .WithStdioServerTransport()
            .WithTools<DuckduckgoTool>()
        |> ignore

        builder.Build().Run()
        0


