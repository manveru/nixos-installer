with builtins;
let
  nixos = import <nixpkgs/nixos> {};
  nixpkgs = import <nixpkgs> {};
  lib = nixpkgs.lib;

  p = val: trace val val;

  buildPath = o: query:
    let
      parts = lib.splitString "." query;
    in
      lib.foldl (s: v: s."${v}") o parts;

  visibleAttrs = attrs:
    lib.filter (a:
      (lib.substring 0 1 a) != "_"
      && a != "functor"
    ) attrs;

  deepAttrValue = value:
    if (isList value) then
      map (v: attrValue v) value
    else if (isAttrs value) then
      if (value ? functor)
      && (value ? name)
      && (value ? description)
      && (value ? typeMerge) then
        value.description
      else
        optionDetails value []
    else
      "[omitted ${typeOf value}]";

  attrValue = value:
    if (isString value)
    || (isInt value)
    || (isBool value)
    || (isNull value) then
      value
    else
      deepAttrValue value;

  optionDetails = o: omit:
    let
      attrs = lib.subtractLists omit (visibleAttrs (attrNames o));
    in
      listToAttrs (map (a:
        {name = a; value = attrValue o."${a}";}
      ) attrs);

  optionAttrs = o:
    visibleAttrs (attrNames o);

  inspect = unknown:
    if (unknown ? value)
    && (unknown ? isDefined) then
      if (unknown.isDefined) then
        optionDetails unknown []
      else
        optionDetails unknown ["value"]
    else
      optionAttrs unknown;
in
  {query ? "options"}:
    inspect (buildPath nixos query)
