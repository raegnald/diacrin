
open Printf

let clamp n lowest highest =
  max lowest (min n highest)


module Layer = struct
  open Bigarray

  type t = (float, float32_elt, c_layout) Array2.t

  let create ~size : t =
    Array2.create float32 c_layout size size

  let size (l: t) =
    Array2.size_in_bytes l

  (** [apply_all f l] applies a function [f] to all values of the layer [l] *)
  let apply_all ~size f (l: t) =
    for i = 0 to size - 1 do
      for j = 0 to size - 1 do
        f l.{i, j}
      done
    done

  let apply_all2 ~size f (l1: t) (l2: t) =
    for i = 0 to size - 1 do
      for j = 0 to size - 1 do
        f l1.{i, j} l2.{i, j}
      done
    done

  let to_ppm ~size ?(scale=1) (l: t) = begin
    let result = ref "" in
    let append s = result := !result ^ s in

    let size' = size * scale in

    append @@ sprintf "P3\n%d %d\n255\n" size' size';

    for i = 0 to size' - 1 do
      for j = 0 to size' - 1 do
        let pixel = l.{i / scale, j / scale} in
        append @@ sprintf "%d %d %d\n"
          (truncate @@ pixel *. 255.) 0 0
      done
    done;

    !result
  end

  (** [to_bin l] will transform the layer [l] into a serialized binary format *)
  let to_bin ~_size (_l: t) = ()

  (** [from_bin s] will transform the serialized binary format [s] into a layer *)
  let from_bin ~_size _s = ()

end


module Perceptron = struct
  let create ~size =
    Layer.create ~size, Layer.create ~size

  let apply_weight_layer ~size input weights =
    assert Layer.(size input = size weights);
    let sum = ref 0. in
    let add_to_sum i w = sum := !sum +. i *. w in
    Layer.apply_all2 ~size add_to_sum input weights;
    !sum

end


module Shape = struct
  let point ~size (layer: Layer.t) x y value =
    assert (x >= 0 && x < size &&
            y >= 0 && y < size);
    layer.{x, y} <- value

  let filled_rect ~size layer x y w h value =
    for i = x to x + w - 1 do
      for j = y to y + h - 1 do
        point ~size layer i j value
      done
    done

  let filled_circle ~size layer cx cy r value = begin
    assert (r > 0);
    let clamp' x = clamp x 0 size in
    let s x = x * x in

    let x0 = clamp' (cx - r)
    and y0 = clamp' (cy - r)
    and x1 = clamp' (cx + r)
    and y1 = clamp' (cy + r) in

    for i = x0 to x1 do
      for j = y0 to y1 do
        let dx = i - cx
        and dy = j - cy in
        if s dx + s dy <= s r then
          point ~size layer i j value
      done
    done
  end

end


let size = 200
let img, weights = Perceptron.create ~size

let () =
  (* Shape.point ~size img 10 10 0.6; *)
  (* Shape.filled_rect ~size img 5 5 10 5 0.6; *)
  Shape.filled_circle ~size img (size/2) (size/2) (size/2 - 1) 0.8;
  Layer.to_ppm ~size img |> print_endline;

  (* Perceptron.apply_weight_layer ~size img weights
    |> Printf.printf "Total: %f\n"; *)
  ()