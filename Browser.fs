module Browser

open System
open System.IO
open System.Diagnostics
open System.Web

let htmlEncode (txt: string) = HttpUtility.HtmlEncode txt
let urlEncode (txt: string) = HttpUtility.UrlEncode txt

type ElementWrapper = seq<string> -> string -> string

// string -> ElementWrapper
let internal plainElement tag txt = sprintf "<%s>%s<%s>" tag txt tag
let internal element tag (cls: string seq) txt =
	let clsAttr =
		if Seq.length cls > 0 then
			sprintf " class='%s'" (cls |> String.concat " ")
		else ""
	sprintf "<%s%s>%s</%s>" tag clsAttr txt tag

let Html = plainElement "html"
let Head = plainElement "head"
let Body = plainElement "body"

let Div  = element "div": ElementWrapper
let Span = element "span": ElementWrapper

let internal Wrap (bodyHtml: string) =
	String.Format (
		Html (
			Head "<link href='/home/js/poker/style.css' rel='stylesheet' type='text/css' />" +
			Body bodyHtml))

let LoadInBrowser html =
	let pageFilename = Path.ChangeExtension (Path.GetTempFileName (), ".html")
	File.WriteAllText (pageFilename, Wrap html)
	let browser = "firefox"
	use p = Process.Start (browser, pageFilename)
	p |> ignore



let list = [1..10] |> List.map (fun x -> Span [] ("Span " + (string x))) |> String.concat "; "
"[" + list + "]" |> LoadInBrowser

let rec htmlize obj =
	if obj = null then ""
	else
		let t = obj.GetType ()
		type FST = FSharpType
		match obj with
		| _ -> when FSharpType.IsTuple t -> 
			FSharpValue.GetTupleFields obj
			|> Seq.map htmlize
			|> Seq.concat " "
		| _ -> when FST.IsUnion t ->
			let (unionCaseInfo, objs) = FSharpValue.GetUnionFields (obj, t)
			objs
			|> Seq.map htmlize
			|> Seq.concat " "
			Div [] unionCaseInfo.Name
		| _ -> when FST.IsRecord t ->
			FSharpValue.GetRecordFields obj
			|> Seq.map htmlize >> Div []
			|> Seq.concat " "
		| _ -> sprintf "%A" obj
