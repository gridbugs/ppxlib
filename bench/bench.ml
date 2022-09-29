(* Run a program on a list of arguments, sending its output to /dev/null,
   returning the wallclock duration of the program in seconds. *)
let time_run_blocking program args =
  let args_arr = Array.of_list (program :: args) in
  let dev_null = Unix.openfile Filename.null [ Unix.O_RDWR ] 0 in
  let timestamp_before = Unix.gettimeofday () in
  let child_pid =
    Unix.create_process program args_arr dev_null dev_null dev_null
  in
  let got_pid, status = Unix.waitpid [] child_pid in
  let timestamp_after = Unix.gettimeofday () in
  Unix.close dev_null;
  if got_pid <> child_pid then failwith "wait returned unexpected pid";
  let () =
    match status with
    | Unix.WEXITED 0 -> ()
    | _ ->
        let command_string = String.concat " " (program :: args) in
        failwith
          (Printf.sprintf "`%s` did not exit successfully" command_string)
  in
  timestamp_after -. timestamp_before

(* Takes the path of a directory and returns a list of the full paths of the
   files contained within it *)
let readdir_full_paths dir =
  Sys.readdir dir |> Array.to_list |> List.sort String.compare
  |> List.map (Filename.concat dir)

module Input = struct
  type t = { path : string }

  let name { path } = Filename.basename path
end

module Driver = struct
  type t = { path : string }

  let name { path } = Filename.basename (Filename.dirname path)
end

module Output = struct
  module Metric = struct
    type t = { name : string; value : Yojson.t; units : string }

    let create ~name ~value ~units = { name; value; units }

    let to_json { name; value; units } : Yojson.t =
      `Assoc
        [ ("name", `String name); ("value", value); ("units", `String units) ]
  end

  module Result = struct
    type t = { name : string; metrics : Metric.t list }

    let create ~name ~metrics = { name; metrics }

    let to_json { name; metrics } : Yojson.t =
      `Assoc
        [
          ("name", `String name);
          ("metrics", `List (List.map Metric.to_json metrics));
        ]
  end

  module Benchmark = struct
    type t = { name : string; results : Result.t list }

    let create ~name ~results = { name; results }

    let to_json { name; results } =
      `Assoc
        [
          ("name", `String name);
          ("benchmarks", `List (List.map Result.to_json results));
        ]
  end

  type t = { benchmarks : Benchmark.t list }
end

module Benchmark = struct
  type t = { driver : Driver.t; input : Input.t }

  let create ~driver ~input = { driver; input }

  let name { driver; input } =
    Printf.sprintf "%s %s" (Driver.name driver) (Input.name input)

  let time_run_blocking { driver; input } =
    let time = time_run_blocking driver.path [ input.path ] in
    let metric =
      Output.Metric.create ~name:"time" ~value:(`Float time) ~units:"seconds"
    in
    ()
end

module Driver_dir = struct
  type t = { path : string }

  let driver_name = "driver.exe"
  let inputs_dir_name = "inputs"
  let driver_path { path } = Filename.concat path driver_name
  let driver t = { Driver.path = driver_path t }
  let inputs_path { path } = Filename.concat path inputs_dir_name

  let of_path path =
    let t = { path } in
    if not (Sys.file_exists (driver_path t)) then
      failwith (Printf.sprintf "failed to find %s in %s" driver_name path);
    if not (Sys.file_exists (inputs_path t)) then
      failwith (Printf.sprintf "failed to find %s in %s" inputs_dir_name path);
    t

  let inputs t =
    readdir_full_paths (inputs_path t) |> List.map (fun path -> { Input.path })

  let benchmarks t =
    let driver = driver t in
    inputs t |> List.map (fun input -> Benchmark.create ~driver ~input)
end

module Path_to_current_exe = struct
  (* This looks up the current executable's path in the proc filesystem which
     is only present on linux. *)
  let via_procfs () =
    let pid = Unix.getpid () in
    let proc_exe_path = Printf.sprintf "/proc/%d/exe" pid in
    if Sys.file_exists proc_exe_path then Some (Unix.readlink proc_exe_path)
    else None

  (* This assumes we were invoked via `make bench` and the cwd is the root of
     the project, and finds the bench directory relative to the current working
     directory. *)
  let via_cwd () =
    let cwd = Unix.getcwd () in
    let relative_path_parts = [ "_build"; "default"; "bench"; "bench.exe" ] in
    let absolute_path =
      List.fold_left Filename.concat cwd relative_path_parts
    in
    if Sys.file_exists absolute_path then Some absolute_path else None

  (* Different methods of getting the path to the current exe in order from
     most reliable to least reliable *)
  let methods = [ via_procfs; via_cwd ]

  (* Tries each method of finding the path to the current exe in order,
     returning the path to the current exe if one of the methods succeeds.
     Fails if no method succeeds. *)
  let get () =
    let maybe_path = List.find_map (fun m -> m ()) methods in
    match maybe_path with
    | Some path -> path
    | None -> failwith "couldn't determine the path to the current exe"
end

module Benchmark_suite = struct
  (* Returns the path to the directory containing the benchmark runner (this
     program) *)
  let get_bench_dir () = Filename.dirname (Path_to_current_exe.get ())
  let drivers_dir_name = "drivers"

  (* Returns the list of ppxlib drivers that will be benchmarked *)
  let get_driver_dirs () =
    let bench_dir = get_bench_dir () in
    readdir_full_paths (Filename.concat bench_dir drivers_dir_name)
    |> List.map Driver_dir.of_path

  let get_benchmarks () =
    get_driver_dirs () |> List.concat_map Driver_dir.benchmarks
end

let () =
  Benchmark_suite.get_benchmarks ()
  |> List.iter (fun benchmark ->
         let name = Benchmark.name benchmark in
         let time = Benchmark.time_run_blocking benchmark in
         print_endline (Printf.sprintf "%s: %f" name time);
         ())
