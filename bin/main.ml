type markdown_node =
  | Heading1 of string
  | Heading2 of string
  | Heading3 of string
  | TextBold of string
  | TextItalics of string
  | BlockQuote of string
  | Text of string
  | Line

let rec in_all f =
  try
    let line = input_line f in
    line :: in_all f
  with End_of_file -> []

class markdown_parser =
  object (self)
    val regex_heading1 = Str.regexp "# +\\(.*\\)"
    val regex_heading2 = Str.regexp "## +\\(.*\\)"
    val regex_heading3 = Str.regexp "### +\\(.*\\)"
    val regex_text_bold = Str.regexp "\\*\\*\\(.*?\\)\\*\\*"
    val regex_text_italics = Str.regexp "\\*\\(.*?\\)\\*"
    val regex_block_quote = Str.regexp "> *\\(.*\\)"
    val regex_line = Str.regexp " *- *- *-.*"

    method match_markdown lines =
      let rec match_markdown_aux = function
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
            :: match_markdown_aux t
      in
      match_markdown_aux lines

    method process_file filename =
      let in_file = open_in filename in
      let contents = in_all in_file in
      close_in in_file;
      let nodes = self#match_markdown contents in
      let html = self#html_of_markdown nodes in
      let filename_no_ext =
        Str.global_replace (Str.regexp "\\.md$") "" filename
      in
      let out_file = open_out (filename_no_ext ^ ".html") in
      output_string out_file html;
      close_out out_file

    method html_of_markdown nodes =
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
  end

let () =
  if Array.length Sys.argv < 2 then
    Printf.printf "Usage: %s file1.md file2.md ...\n" Sys.argv.(0)
  else
    let threads = ref [] in
    for i = 1 to Array.length Sys.argv - 1 do
      let filename = Sys.argv.(i) in
      let parser = new markdown_parser in
      let thread = Thread.create parser#process_file filename in
      threads := thread :: !threads
    done;
    List.iter Thread.join !threads
