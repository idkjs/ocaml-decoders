module My_result: {
  type t('good, 'bad) =
    Belt.Result.t('good, 'bad) = | Ok('good) | Error('bad);

  let return: 'good => t('good, 'bad);

  let map: ('a => 'b, t('a, 'err)) => t('b, 'err);

  let map_err: ('err1 => 'err2, t('a, 'err1)) => t('a, 'err2);

  module Infix: {
    let (>|=): (t('a, 'err), 'a => 'b) => t('b, 'err);

    let (>>=): (t('a, 'err), 'a => t('b, 'err)) => t('b, 'err);
  };
};

module My_opt: {
  let return: 'a => option('a);

  let flat_map: ('a => option('b), option('a)) => option('b);
};

module My_list: {
  let take: (int, list('a)) => list('a);

  let map: ('a => 'b, list('a)) => list('b);

  let mapi: ((int, 'a) => 'b, list('a)) => list('b);

  let find_map: ('a => option('b), list('a)) => option('b);

  let fold_left: (('a, 'b) => 'a, 'a, list('b)) => 'a;
};
