open Universe
open Code
open Color

let displayWidth = 800.
let displayHeight = 800.

type vector = { x : float
              ; y : float
              }

type shape =
    | Circle  of {radius : float ; color  : Color.t}
    | Polygon of {constructionList : (float * float) list ; color : Color.t}



type obj =  { mass          : float
            ; shape         : shape
            ; location      : vector
            ; velocity      : vector
            ; acceleration  : vector
            ; forcesUpon    : (vector * obj) list
            ; forcesAgainst : (vector * obj) list
            ; permanent     : bool
            ; elasticity    : float
            }

type force = { vector    : vector
             ; affecting : obj
             }


type model = { time       : float
             ; background : Image.t
             ; objectList    : obj list
             }


(* GEOMETRY/MATH AREA
---------------------------------------------------------------------------------------------
*)

let regPolygonSideLength sides area =
  let n = Code.i2F sides in
  let tan x = sin (x) /. cos (x) in
  (4. *. area *. tan (Code.pi /. n) /. n) ** 0.5


let regPolygonMaker sides sideLength =
  let angleIncrement = Code.pi -. (Code.i2F (sides - 2) *. Code.pi /. Code.i2F (sides)) in
  let rec loop counter =
    match counter = -sides with
    | true -> []
    | false -> (sideLength *. cos (angleIncrement *. Code.i2F (counter)),
                sideLength *. sin (angleIncrement *. Code.i2F (counter)))
                :: loop (counter - 1)
  in
  loop 0

let rec vectorAdd (vectors : vector list) =
  match vectors with
  | [] -> {x = 0. ; y = 0.}
  | v::vs -> {x = v.x +. (vectorAdd vs).x ; y = v.y +. (vectorAdd vs).y}

let rec vectorCMul (v : vector) (c : float) =
  {x = v.x *. c ; y = v.y *. c}

    (*
--------------------------------------------------------------------------------------------
*)


let g = 9.81

let getFG (obj : obj) =
  g *. obj.mass



let getAcceleration (obj : obj) =
  vectorAdd force

let collision (obj1 : obj) (obj2 : obj) (angle : float) =
  if obj2.permanent
  then {x = }


let imageOf (obj : obj) =
  match obj.shape with
  | Circle {radius ; color}               -> Image.circle radius color
  | OtherShape {constructionList ; color} -> Image.polygon constructionList color

let background =
    Image.place_image
    (Image.rectangle displayWidth (displayHeight /. 8.) Color.brown)
    (0., displayHeight *. 7. /. 8.)
    (Image.rectangle displayWidth displayHeight Color.skyBlue)

let rec modelListMaker model record =
  match model with
  | []   -> []
  | h::t -> h.record :: modelListMaker t record

let view model =
  let locations     = modelListMaker model.objectList location in
  let velocities    = modelListMaker model.objectList velocity in
  let accelerations = modelListMaker model.objectList



let go model  =
  World.big_bang model
    ~name: "2D Physics Simulation"
    ~width: (Code.f2I displayWidth)
    ~height: (Code.f2I displayHeight)
    ~to_draw: (view)
    ~on_tick: (update)
    ~stop_when: finished
    ~to_draw_last: view
    ~rate: 0.03125
