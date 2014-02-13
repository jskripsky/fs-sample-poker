module Browser

open System
open System.IO
open System.Diagnostics
open System.Web

let htmlEncode (txt: string) = HttpUtility.HtmlEncode txt
let urlEncode (txt: string) = HttpUtility.UrlEncode txt

let internal element tag (cls: string seq) txt = sprintf "<%s class='%s'>%s</%s>" tag (cls |> String.concat " ") txt tag

type ElementWrapper = seq<string> -> string -> string
let Div  = element "div": ElementWrapper
let Span = element "span": ElementWrapper

let internal Wrap (bodyHtml: string) = String.Format ("<html><head><link href='/home/js/poker/style.css' rel='stylesheet' type='text/css' /><body>\n{0}\n</body></html>", bodyHtml)

let LoadInBrowser html =
	let pageFilename = Path.ChangeExtension (Path.GetTempFileName (), ".html")
	File.WriteAllText (pageFilename, Wrap html)
	let browser = "firefox"
	use p = Process.Start (browser, pageFilename)
	p |> ignore


/// [1..10] |> List.map (fun x -> sprintf "<div>%s</div>" (string x)) |> String.concat "\n" |> LoadInBrowser
