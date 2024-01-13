(** Replace extension of a file
    @param ext new extension without the dot *)
let replace_extension filename ext =
  Filename.remove_extension filename ^ "." ^ ext


(** Make the ouput path based on input path.

    The output path is based on current work directory(cwd) and input filename:
    cwd/filename.ll

    @param input_path the whole path of the input file
    @return cwd/input_filename.ll *)
let make_output_path input_path =
  let cwd = Sys.getcwd () in
  let input_filename = Filename.basename input_path in
  let output_filename = replace_extension input_filename "ll" in
  Filename.concat cwd output_filename
