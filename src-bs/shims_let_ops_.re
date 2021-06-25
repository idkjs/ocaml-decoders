/* Note: copied from src/gen/mkshims.ml */
module type S = {type t_let('a);};

module Make = (X: {type t('a);}) => {
  type t_let('a) = X.t('a);
};

module type S2 = {type t_let2('a, 'b);};

module Make2 = (X: {type t('a, 'b);}) => {
  type t_let2('a, 'b) = X.t('a, 'b);
};
