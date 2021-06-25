/** {2 Ocyaml implementation} */;

open Decoders;

module Yaml_decodeable: Decode.Decodeable with type value = Ocyaml.yaml = {
  open Ocyaml;

  type value = yaml;

  let rec pp = fmt =>
    fun
    | Scalar(string) => Format.fprintf(fmt, "@[%S@]", string)
    | Collection(xs) =>
      Format.fprintf(
        fmt,
        "@[%a@]",
        Format.pp_print_list((fmt, yaml) =>
          Format.fprintf(fmt, "- @[%a@]", pp, yaml)
        ),
        xs,
      )
    | Structure(xs) =>
      Format.fprintf(
        fmt,
        "@[%a@]",
        Format.pp_print_list((fmt, (key, value)) =>
          Format.fprintf(fmt, "@[%a@]:@ @[%a@]", pp, key, pp, value)
        ),
        xs,
      );

  let of_string: string => result(value, string) = (
    string =>
      try(Ok(Ocyaml.of_string(string))) {
      | exn => Error(Printexc.to_string(exn))
      }:
      string => result(value, string)
  );

  let of_file = file =>
    try(Ok(Ocyaml.of_file(file))) {
    | e => Error(Printexc.to_string(e))
    };

  let get_string: value => option(string) = (
    fun
    | Scalar(value) => Some(value)
    | _ => None:
      value => option(string)
  );

  let get_int: value => option(int) = (
    t =>
      try(get_string(t) |> Decoders_util.My_opt.map(int_of_string)) {
      | Failure(_) => None
      }:
      value => option(int)
  );

  let get_float: value => option(float) = (
    t =>
      try(get_string(t) |> Decoders_util.My_opt.map(float_of_string)) {
      | Failure(_) => None
      }:
      value => option(float)
  );

  let get_bool: value => option(bool) = (
    t =>
      try(get_string(t) |> Decoders_util.My_opt.map(bool_of_string)) {
      | Failure(_) => None
      }:
      value => option(bool)
  );

  let get_null: value => option(unit) = (
    t =>
      get_string(t)
      |> Decoders_util.My_opt.flat_map(
           fun
           | "" => Some()
           | _ => None,
         ):
      value => option(unit)
  );

  let get_list =
    fun
    | Collection(l) => Some(l)
    | _ => None;

  let get_key_value_pairs =
    fun
    | Structure(assoc) => Some(assoc)
    | _ => None;

  let to_list = values => Collection(values);
};

include Decode.Make(Yaml_decodeable);
