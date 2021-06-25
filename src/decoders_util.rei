/** Util module used for native builds (excluded in bs-config.json) */

module My_result: {
  type t('good, 'bad) = result('good, 'bad) = | Ok('good) | Error('bad);

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

  let map: ('a => 'b, option('a)) => option('b);

  let flat_map: ('a => option('b), option('a)) => option('b);
};

module My_list: {
  let take: (int, list('a)) => list('a);

  let all_some: list(option('a)) => option(list('a));

  let map: ('a => 'b, list('a)) => list('b);

  let mapi: ((int, 'a) => 'b, list('a)) => list('b);

  let filter_map: ('a => option('b), list('a)) => list('b);

  let find_map: ('a => option('b), list('a)) => option('b);

  let fold_left: (('a, 'b) => 'a, 'a, list('b)) => 'a;

  let append: (list('a), list('a)) => list('a);

  let (@): (list('a), list('a)) => list('a);

  let flat_map: ('a => list('b), list('a)) => list('b);
};

let with_file_in: (string, in_channel => 'a) => 'a;

let read_all: in_channel => string;
