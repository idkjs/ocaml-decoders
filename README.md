# ocaml-decoders: Elm-inspired decoders for OCaml [![build status](https://travis-ci.org/mattjbray/ocaml-decoders.svg?branch=master)](https://travis-ci.org/mattjbray/ocaml-decoders)

A combinator library for "decoding" JSON-like values into your own OCaml types, inspired by Elm's `Json.Decode` and `Json.Encode`.

> Eh?

An OCaml program having a JSON (or YAML) data source usually goes something like this:

1. Get your data from somewhere. Now you have a `string`.
2. *Parse* the `string` as JSON (or YAML). Now you have a `Yojson.Basic.json`, or maybe an `Ezjsonm.value`.
3. *Decode* the JSON value to an OCaml type that's actually useful for your program's domain.

This library helps with step 3.

# Getting started

Install one of the supported decoder backends:

### For ocaml

```
opam install decoders-bencode      # For bencode
opam install decoders-cbor         # For CBOR
opam install decoders-ezjsonm      # For ezjsonm
opam install decoders-jsonm        # For jsonm
opam install decoders-msgpck       # For msgpck
opam install decoders-sexplib      # For sexplib
opam install decoders-yojson       # For yojson
```

### For bucklescript

```
npm install --save-dev bs-decoders
```

## Decoding

Now we can start decoding stuff!

First, a module alias to save some keystrokes. In this guide, we'll parse JSON
using `Yojson`'s `Basic` variant.

```reason
rtop # module D = Decoders_yojson.Basic.Decode;
module D = Decoders_yojson.Basic.Decode
```

Let's set our sights high and decode an integer.

```reason
rtop # D.decode_value(D.int, `Int(1));
- : (int, error) result = Ok 1
```

Nice! We used `decode_value`, which takes a `decoder` and a `value` (in this
case a `Yojson.Basic.json`) and... decodes the value.

```reason
rtop # D.decode_value;
- : 'a decoder -> value -> ('a, error) result = <fun>
```

For convenience we also have `decode_string`, which takes a `string` and calls
`Yojson`'s parser under the hood.

```reason
rtop # D.decode_string(D.int, "1");
- : (int, error) result = Ok 1
```

What about a `list` of `int`s? Here's where the "combinator" part comes in.

```reason
rtop # D.decode_string(D.(list(int)), "[1,2,3]");
- : (int list, error) result = Ok [1; 2; 3]
```

Success!

Ok, so what if we get some unexpected JSON?

```reason
rtop # #install_printer D.pp_error;;
rtop # D.decode_string(D.(list(int)), "[1,2,true]");
- : (int list, error) result =
Error while decoding a list: element 2: Expected an int, but got true
```

## Complicated JSON structure

To decode a JSON object with many fields, we can use the let-binding operators
(`let*`, etc.) from the `Infix` module.

```reason
type my_user = {
  name: string,
  age: int,
};

let my_user_decoder: decoder(my_user) = (
  {
    open D;
    let* name = field("name", string);
    let* age = field("age", int);
    succeed({name, age});
  }:
    decoder(my_user)
);
```

> *Note for Bucklescript users*: let-binding operators are not currently available
> in Bucklescript, so if you need your decoders to be compatible with Bucklescript
> you can use the monadic bind operator (`>>=`):
>
> ```reason
> let my_user_decoder: decoder(my_user) = (
>  D.(
>    field("name", string)
>    >>= (name => field("age", int) >>= (age => succeed({name, age})))
>  ):
>    decoder(my_user)
>);
> ```


We can also use these operators to decode objects with inconsistent structure. Say, for
example, our JSON is a list of shapes. Squares have a side length, circles have
a radius, and triangles have a base and a height.

```json
[{ "shape": "square", "side": 11 },
 { "shape": "circle", "radius": 5 },
 { "shape": "triange", "base": 3, "height": 7 }]
```

We could represent these types in OCaml and decode them like this:

```reason
type shape =
  | Square(int)
  | Circle(int)
  | Triangle(int, int);

let square_decoder: decoder(shape) = (
  {
    open D;
    let+ s = field("side", int);
    Square(s);
  }:
    decoder(shape)
);

let circle_decoder: decoder(shape) = (
  {
    open D;
    let+ r = field("radius", int);
    Circle(r);
  }:
    decoder(shape)
);
```

Now, say that we didn't have the benefit of the `"shape"` field describing the
type of the shape in our JSON list. We can still decode the shapes by trying
each decoder in turn using the `one_of` combinator.

`one_of` takes a list of `string * 'a decoder` pairs and tries each decoder in
turn. The `string` element of each pair is just used to name the decoder in
error messages.

```reason
let shape_decoder_2 : shape decoder =
  D.(
    one_of
      [ ("a square", square_decoder)
      ; ("a circle", circle_decoder)
      ; ("a triangle", triangle_decoder)
      ]
  )
```

## Generic decoders


Suppose our program deals with users and roles. We want to decode our JSON input
into these types.

```reason
type role =
  | Admin
  | User;

type user = {
  name: string,
  roles: list(role),
};
```

Let's define our decoders. We'll write a module functor so we can re-use the
same decoders across different JSON libraries, with YAML input, or with
Bucklescript.

```reason
module My_decoders = (D: Decoders.Decode.S) => {
  open D;

  let role: decoder(role) = (
    string
    >>= (
      fun
      | "ADMIN" => succeed(Admin)
      | "USER" => succeed(User)
      | _ => fail("Expected a role")
    ):
      decoder(role)
  );

  let user: decoder(user) = (
    {
      let* name = field("name", string);
      let* roles = field("roles", list(role));
      succeed({name, roles});
    }:
      decoder(user)
  );
};

module My_yojson_decoders = My_decoders(Decoders_yojson.Basic.Decode);
```

Great! Let's try them out.

```reason
rtop # open My_yojson_decoders;
rtop # D.decode_string(role, {| "USER" |});
- : (role, error) result = Ok User

rtop # D.decode_string(D.(field("users", list(user))));
         {| {"users": [{"name": "Alice", "roles": ["ADMIN", "USER"]},
                       {"name": "Bob", "roles": ["USER"]}]}
          |};;
- : (user list, error) result =
Ok [{name = "Alice"; roles = [Admin; User]}; {name = "Bob"; roles = [User]}]
```

Let's introduce an error in the JSON:

```reason
rtop # D.decode_string(D.(field("users", list(user))));
         {| {"users": [{"name": "Alice", "roles": ["ADMIN", "USER"]},
                       {"name": "Bob", "roles": ["SUPER_USER"]}]}
          |};;
- : (user list, error) result =
Error
 in field "users":
   while decoding a list:
     element 1:
       in field "roles":
         while decoding a list:
           element 0: Expected a role, but got "SUPER_USER"
```

We get a nice pointer that we forgot to handle the `SUPER_USER` role.

## Encoding

`ocaml-decoders` also has support for defining backend-agnostic encoders, for
turning your OCaml values into JSON values.

```reason
module My_yojson_encoders = My_encoders(Decoders_yojson.Basic.Encode);

module E = Decoders_yojson.Basic.Encode;
open My_yojson_encoders;
let users = [
  {name: "Alice", roles: [Admin, User]},
  {name: "Bob", roles: [User]},
];

E.encode_string(E.obj, [("users", E.list(user, users))]);
```

```reason
rtop # module E = Decoders_yojson.Basic.Encode;
rtop # open My_yojson_encoders;
rtop # let users = [
  {name: "Alice", roles: [Admin, User]},
  {name: "Bob", roles: [User]},
];

rtop # E.encode_string(E.obj, [("users", E.list(user, users))]);
- : string =
"{\"users\":[{\"name\":\"Alice\",\"roles\":[\"ADMIN\",\"USER\"]},{\"name\":\"Bob\",\"roles\":[\"USER\"]}]}"
```

## API Documentation

For more details, see the API documentation:

* [`Decoders.Decode.S`](https://mattjbray.github.io/ocaml-decoders/decoders/Decoders/Decode/module-type-S/index.html) interface
* [`Decoders.Encode.S`](https://mattjbray.github.io/ocaml-decoders/decoders/Decoders/Encode/module-type-S/index.html) interface

# Release

After updating CHANGES.md:

```
npm version <newversion>
git push --tags
dune-release --name decoders
npm publish
```
