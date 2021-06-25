module My_result = {
  type t('good, 'bad) =
    Belt.Result.t('good, 'bad) = | Ok('good) | Error('bad);

  let return = x => Ok(x);

  let map: ('a => 'b, t('a, 'err)) => t('b, 'err) = (
    (f, x) => Belt.Result.map(x, f): ('a => 'b, t('a, 'err)) => t('b, 'err)
  );

  let map_err: ('err1 => 'err2, t('a, 'err1)) => t('a, 'err2) = (
    f =>
      fun
      | Ok(x) => Ok(x)
      | Error(e) => Error(f(e)):
      ('err1 => 'err2, t('a, 'err1)) => t('a, 'err2)
  );

  module Infix = {
    let (>|=): (t('a, 'err), 'a => 'b) => t('b, 'err) = (
      Belt.Result.map: (t('a, 'err), 'a => 'b) => t('b, 'err)
    );

    let (>>=): (t('a, 'err), 'a => t('b, 'err)) => t('b, 'err) = (
      Belt.Result.flatMap: (t('a, 'err), 'a => t('b, 'err)) => t('b, 'err)
    );
  };
};

module My_opt = {
  let return = x => Some(x);

  let flat_map = (f, x) => Belt.Option.flatMap(x, f);
};

module My_list = {
  let take = (i, xs) =>
    xs->(Belt.List.take(i))->(Belt.Option.getWithDefault([]));

  let map = (f, xs) => Belt.List.map(xs, f);

  let mapi = (f, xs) => Belt.List.mapWithIndex(xs, f);

  let find_map = (f, xs) =>
    xs
    ->(
        Belt.List.getBy(x =>
          switch (f(x)) {
          | Some(_) => true
          | None => false
          }
        )
      )
    ->(Belt.Option.flatMap(f));

  let fold_left = (f, init, xs) => Belt.List.reduce(xs, init, f);
};
