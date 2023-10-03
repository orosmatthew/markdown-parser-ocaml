let regex_heading1 = Str.regexp "# +\\(.*\\)"
let regex_heading2 = Str.regexp "## +\\(.*\\)"
let regex_heading3 = Str.regexp "### +\\(.*\\)"
let regex_text_bold = Str.regexp "\\*\\*\\(.*?\\)\\*\\*"
let regex_text_italics = Str.regexp "\\*\\(.*?\\)\\*"
let regex_block_quote = Str.regexp "> *\\(.*\\)"
let regex_line = Str.regexp " *- *- *-.*"

type markdown_node =
  | Heading1 of string
  | Heading2 of string
  | Heading3 of string
  | TextBold of string
  | TextItalics of string
  | BlockQuote of string
  | Text of string
  | Line

let rec match_markdown = function
  | [] -> []
  | h :: t ->
      (if Str.string_match regex_heading1 h 0 then
         Heading1 (Str.matched_group 1 h)
       else if Str.string_match regex_heading2 h 0 then
         Heading2 (Str.matched_group 1 h)
       else if Str.string_match regex_heading3 h 0 then
         Heading3 (Str.matched_group 1 h)
       else if Str.string_match regex_text_bold h 0 then
         TextBold (Str.matched_group 1 h)
       else if Str.string_match regex_text_italics h 0 then
         TextItalics (Str.matched_group 1 h)
       else if Str.string_match regex_block_quote h 0 then
         BlockQuote (Str.matched_group 1 h)
       else if Str.string_match regex_line h 0 then Line
       else Text h)
      :: match_markdown t

let rec in_all f =
  try
    let line = input_line f in
    line :: in_all f
  with End_of_file -> []

let html_of_markdown nodes =
  let rec aux = function
    | [] -> []
    | h :: t ->
        (match h with
        | Heading1 s -> Printf.sprintf "<h1>%s</h1>" s
        | Heading2 s -> Printf.sprintf "<h2>%s</h2>" s
        | Heading3 s -> Printf.sprintf "<h3>%s</h3>" s
        | TextBold s -> Printf.sprintf "<b>%s</b><br />" s
        | TextItalics s -> Printf.sprintf "<i>%s</i><br />" s
        | BlockQuote s -> Printf.sprintf "<blockquote>%s</blockquote>" s
        | Line -> "<hr />"
        | Text s -> s)
        :: aux t
  in
  let rec concat_lines = function
    | [] -> ""
    | h :: t -> h ^ "\n" ^ concat_lines t
  in
  let lines = aux nodes in
  concat_lines lines

let () =
  let in_file = open_in "test.md" in
  let contents = in_all in_file in
  close_in in_file;
  let nodes = match_markdown contents in
  let html = html_of_markdown nodes in
  let out_file = open_out "test.html" in
  output_string out_file html;
  close_out out_file
