open System
open System.Diagnostics
open System.IO
open System.Net.Http
open System.Linq
        
let GetIndexes(item: string) =
    let mutable i = 0
    [for _ in 0..item.Count(fun f -> f = '.') do
        let current = item.IndexOf('.', if i = 0 then 0 else i + 1)
        i <- current
        if i >= 0 then yield current]

let ProcessItem(indexes: int list,
                l: string) =
    if (indexes.Length <> 2) then
        let secondTop = l.[(indexes.[^2] + 1)..]
        if (secondTop.Length > 3) then secondTop
        else l.[(indexes.[^3] + 1)..]
    else
        let item = l.[(indexes.[0] + 1)..indexes.[1]]
        if item.Length <= 3 then l
        else l.[(indexes.[0] + 1)..]

let SortDnsList(dnsList,
                distinct) =
    let enumerable = 
        if distinct then dnsList |> Seq.distinct
        else dnsList

    enumerable |> Seq.sortBy (fun x ->
            let indexes = GetIndexes(x)
            if (indexes.Length <= 1) then x
            else ProcessItem(indexes, x)
            , x.Length
    )

let getAsync (client: HttpClient) (url: string) = 
    async {
        let! response = client.GetAsync(url) |> Async.AwaitTask
        response.EnsureSuccessStatusCode () |> ignore
        let! content = response.Content.ReadAsStringAsync() |> Async.AwaitTask
        return Seq.toArray (content.Split('\n', StringSplitOptions.RemoveEmptyEntries))
    }

let wwwPrefix = "www."
let replaceWww(s: ReadOnlySpan<char>) =
    if s.StartsWith(wwwPrefix.AsSpan()) then s.Slice wwwPrefix.Length
    else s

let replace(s: ReadOnlySpan<char>) =
    replaceWww(
        if s.Contains('#') then s.Slice(0, s.IndexOf('#')).Slice(8)
        else s.Slice(8)
        ).Trim()

let GetSourceDnsList(httpClient: HttpClient) =
    let loopBackEntry = "0.0.0.0 0.0.0.0"
    let includePrefix = "0.0.0.0 "
    async {
        let url = "https://raw.githubusercontent.com/StevenBlack/hosts/master/alternates/fakenews-gambling-porn/hosts"
        let! reqResult = getAsync httpClient url
        let filtered =
            reqResult
            |> Array.filter(fun x -> x <> loopBackEntry && x.StartsWith(includePrefix))
            |> Array.map(fun x -> replace(x.AsSpan()).ToString())
        
        return SortDnsList(filtered, false)
    }

let GetAdGuardDnsList(httpClient: HttpClient) =
    async {
        let url = "https://adguardteam.github.io/AdGuardSDNSFilter/Filters/filter.txt"
        let! reqResult = getAsync httpClient url
        let filtered =
            reqResult
            |> Array.filter(fun x -> x.AsSpan().[0] = '|' && x.AsSpan().Contains('*') = false)
            |> Array.map(fun x -> replaceWww(x.[2..^1].AsSpan().Trim()).ToString())
        
        return SortDnsList(filtered, false)
    }
    
let knownBad = ["000webhostapp.com"
                "001www.com"
                "1stok.com"
                "24.eu"
                "2o7.net"
                "about-tabs.com"
                "actonservice.com"
                "ad.tomshardware.com"
                "adtech.fr"
                "adtech.us"
                "angelcities.com"
                "bongacams.org"
                "bravenet.com"
                "cdn3x.com"
                "daraz.com"
                "dditscdn.com"
                "ddns.name"
                "hitbox.com"
                "intellitxt.com"
                "p2l.info"
                "sextracker.be"
                "sextracker.de"]
let knownBadCacheEntries = knownBad |> Seq.map (fun a -> a, "." + a ) |> dict

let trash(combined: string array) = [|for i in combined do
                                         if knownBadCacheEntries.Any(fun x -> i.EndsWith x.Value)
                                         then yield i|] |> Seq.toArray

[<EntryPoint>]
let main _ =
    let stopWatch = Stopwatch()
    use httpClient = new HttpClient()
    stopWatch.Start()
    let sourceDnsList = GetSourceDnsList(httpClient) |> Async.RunSynchronously
    let adGuardDnsList = GetAdGuardDnsList(httpClient) |> Async.RunSynchronously

    let mutable combined = SortDnsList (Seq.append adGuardDnsList sourceDnsList, true) |> Seq.toArray

    let trash = trash(combined)
    combined <- SortDnsList (Seq.except trash combined |> Seq.append knownBad, true) |> Seq.toArray
    let cachedEntries = combined |> Seq.map (fun a -> a, "." + a ) |> dict
    let resizeArray = ResizeArray(combined.Length)
    
    let processItem(position, item: string, loopBack) =
        let realLoopback = if position < loopBack then 0 else position - loopBack
        for i = position downto realLoopback do
            let otherItem = combined.[i]
            if (otherItem.Length + 1 < item.Length
                && otherItem <> item
                && cachedEntries.ContainsKey otherItem
                && item.EndsWith cachedEntries.[otherItem]) then resizeArray.Add(item)
    
    let mutable round = 0
    let mutable firstRun = true
    let mutable shouldContinue = true
    while shouldContinue do
        round <- round + 1
        let currentLoopBack = 250*round
        resizeArray.Clear()
        Array.Parallel.iteri (fun i x -> processItem(i, x, currentLoopBack)) combined
        shouldContinue <- resizeArray.Count > 0
        if firstRun then combined <- SortDnsList(combined |> Seq.except adGuardDnsList |> Seq.except resizeArray, false) |> Seq.toArray
        else combined <- SortDnsList(combined |> Seq.except resizeArray, false) |> Seq.toArray
        firstRun <- false

    stopWatch.Stop()
    printfn $"{stopWatch.Elapsed}"
    printfn $"{combined.Length}"
    
    File.WriteAllLines("hosts", combined)
    
    0