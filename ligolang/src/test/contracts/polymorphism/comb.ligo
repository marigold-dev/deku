type dup(a) is a * a

function diag(const x : _a) : dup(_a) is (x, x)

function rev(const xs : list (_a)) : list (_a) is block {
  var zs := (nil : list (_a));
  for x in list xs block {
    zs := x # zs;
  };
} with zs

function zip(const xs : list(_a); var ys : list(_b)) : list(_a * _b) is block {
  var zs := (nil : list(_a * _b));
  for x in list xs block {
    var t := case ys of
      | nil -> (failwith("error") : _b * list(_b))
      | (y # ys) -> (y, ys)
    end;
    zs := ((x, t.0) # zs);
    ys := t.1;
  };
  if List.length(ys) > 0n then failwith("error") else skip;
  zs := rev(zs);
} with zs

function self_zip(const xs : list(_a)) : list(_a * _a) is block {
  const (xs, ys) = diag(xs);
} with zip(xs, ys)

const v = self_zip(list ["a";"b"])
const w = zip(list [1;2;3], list [4n;5n;6n])
