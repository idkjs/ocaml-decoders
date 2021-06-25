/** Util module used for native builds (excluded in bs-config.json) */
module My_result = {
  type t('good, 'bad) = result('good, 'bad) = | Ok('good) | Error('bad);

  let return = x => Ok(x);

  let map = (f, e) =>
    switch (e) {
    | Ok(x) => Ok(f(x))
    | Error(s) => Error(s)
    };

  let map_err = (f, e) =>
    switch (e) {
    | Ok(_) as res => res
    | Error(y) => Error(f(y))
    };

  let flat_map = (f, e) =>
    switch (e) {
    | Ok(x) => f(x)
    | Error(s) => Error(s)
    };

  module Infix = {
    let (>|=) = (e, f) => map(f, e);

    let (>>=) = (e, f) => flat_map(f, e);
  };
};

module My_opt = {
  let return = x => Some(x);

  let map = f =>
    fun
    | None => None
    | Some(x) => Some(f(x));

  let flat_map = (f, o) =>
    switch (o) {
    | None => None
    | Some(x) => f(x)
    };
};

module My_list = {
  let take = (n, l) => {
    let rec direct = (i, n, l) =>
      switch (l) {
      | [] => []
      | _ when i == 0 => safe(n, [], l)
      | [x, ...l'] =>
        if (n > 0) {
          [x, ...direct(i - 1, n - 1, l')];
        } else {
          [];
        }
      }
    and safe = (n, acc, l) =>
      switch (l) {
      | [] => List.rev(acc)
      | _ when n == 0 => List.rev(acc)
      | [x, ...l'] => safe(n - 1, [x, ...acc], l')
      };

    direct(500, n, l);
  };

  let map = (f, l) => {
    let rec direct = (f, i, l) =>
      switch (l) {
      | [] => []
      | [x] => [f(x)]
      | [x1, x2] =>
        let y1 = f(x1);
        [y1, f(x2)];
      | [x1, x2, x3] =>
        let y1 = f(x1);
        let y2 = f(x2);
        [y1, y2, f(x3)];
      | _ when i == 0 => List.rev(List.rev_map(f, l))
      | [x1, x2, x3, x4, ...l'] =>
        let y1 = f(x1);
        let y2 = f(x2);
        let y3 = f(x3);
        let y4 = f(x4);
        [y1, y2, y3, y4, ...direct(f, i - 1, l')];
      };

    direct(f, 500, l);
  };

  let all_some = l =>
    try(
      Some(
        map(
          fun
          | Some(x) => x
          | None => raise(Exit),
          l,
        ),
      )
    ) {
    | Exit => None
    };

  let mapi = (f, l) => {
    let r = ref(0);
    map(
      x => {
        let y = f(r^, x);
        incr(r);
        y;
      },
      l,
    );
  };

  let find_map = (f, l) => {
    let rec aux = f =>
      fun
      | [] => None
      | [x, ...l'] =>
        switch (f(x)) {
        | Some(_) as res => res
        | None => aux(f, l')
        };

    aux(f, l);
  };

  let filter_map = (f, l) => {
    let rec recurse = (acc, l) =>
      switch (l) {
      | [] => List.rev(acc)
      | [x, ...l'] =>
        let acc' =
          switch (f(x)) {
          | None => acc
          | Some(y) => [y, ...acc]
          };
        recurse(acc', l');
      };

    recurse([], l);
  };

  let fold_left = List.fold_left;

  let direct_depth_append_ = 10_000;

  let append = (l1, l2) => {
    let rec direct = (i, l1, l2) =>
      switch (l1) {
      | [] => l2
      | _ when i == 0 => safe(l1, l2)
      | [x, ...l1'] => [x, ...direct(i - 1, l1', l2)]
      }
    and safe = (l1, l2) => List.rev_append(List.rev(l1), l2);
    switch (l1) {
    | [] => l2
    | [x] => [x, ...l2]
    | [x, y] => [x, y, ...l2]
    | _ => direct(direct_depth_append_, l1, l2)
    };
  };

  let (@) = append;

  let flat_map = (f, l) => {
    let rec aux = (f, l, kont) =>
      switch (l) {
      | [] => kont([])
      | [x, ...l'] =>
        let y = f(x);
        let kont' = tail =>
          switch (y) {
          | [] => kont(tail)
          | [x] => kont([x, ...tail])
          | [x, y] => kont([x, y, ...tail])
          | l => kont(append(l, tail))
          };

        aux(f, l', kont');
      };

    aux(f, l, l => l);
  };
};

let with_file_in = (file, f) => {
  let ic = open_in(file);
  try({
    let res = f(ic);
    close_in(ic);
    res;
  }) {
  | e =>
    close_in_noerr(ic);
    raise(e);
  };
};

let read_all = (ic): string => {
  let buf = ref(Bytes.create(2048));
  let len = ref(0);
  try(
    {
      while (true) {
        /* resize */
        if (len^ == Bytes.length(buf^)) {
          buf := Bytes.extend(buf^, 0, len^);
        };
        assert(Bytes.length(buf^) > len^);
        let n = input(ic, buf^, len^, Bytes.length(buf^) - len^);
        len := len^ + n;
        if (n == 0) {
          raise(Exit);
        };
      };
      /* exhausted */
      assert(false);
    }
  ) {
  /* never reached*/

  | Exit => Bytes.sub_string(buf^, 0, len^)
  };
};
