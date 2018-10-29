[@bs.deriving abstract]
type imageData = {
  data: Js.Typed_array.Uint8ClampedArray.t,
  width: int,
  height: int,
};

module Ctx = {
  type t;
  type imageSource = Dom.element;

  [@bs.set] external setFillStyle : (t, string) => unit = "fillStyle";

  [@bs.set] external setStrokeStyle : (t, string) => unit = "strokeStyle";

  [@bs.set] external setStrokeWidth : (t, int) => unit = "lineWidth";

  [@bs.set] external setLineWidth : (t, int) => unit = "lineWidth";

  [@bs.set] external setLineCap : (t, string) => unit = "lineCap";

  [@bs.send] external fillRect : (t, int, int, int, int) => unit = "";

  [@bs.send] external strokeRect : (t, int, int, int, int) => unit = "";

  [@bs.send] external strokeText : (t, string, int, int) => unit = "";

  [@bs.send] external clearRect : (t, int, int, int, int) => unit = "";

  [@bs.send] external fillText : (t, string, int, int) => unit = "";

  [@bs.set] external setFont : (t, string) => unit = "font";

  [@bs.set] external setTextAlign : (t, string) => unit = "textAlign";

  [@bs.set] external setTextBaseline : (t, string) => unit = "textBaseline";

  [@bs.send]
  external drawImage : (t, ~image: imageSource, ~dx: int, ~dy: int) => unit =
    "";

  [@bs.send]
  external drawImageDestRect :
    (t, ~image: imageSource, ~dx: int, ~dy: int, ~dw: int, ~dh: int) => unit =
    "drawImage";

  [@bs.send]
  external createImageData : (t, ~w: int, ~h: int) => imageData = "";

  [@bs.send]
  external getImageData :
    (t, ~sx: int, ~sy: int, ~sw: int, ~sh: int) => imageData =
    "";

  [@bs.send]
  external putImageData :
    (t, ~imageData: imageData, ~dx: int, ~dy: int) => unit =
    "";

  let setImageSmoothing: (t, bool) => unit = [%bs.raw
    (ctx, v) => {|
       ctx.mozImageSmoothingEnabled = v;
       ctx.oImageSmoothingEnabled = v;
       ctx.webkitImageSmoothingEnabled = v;
       ctx.msImageSmoothingEnabled = v;
       ctx.imageSmoothingEnabled = v;
       |}
  ];
};

[@bs.send]
external getContext : (Dom.element, [@bs.as "2d"] _) => Ctx.t = "getContext";

[@bs.send]
external getContextWithAttrs :
  (Dom.element, [@bs.as "2d"] _, Js.t('a)) => Ctx.t =
  "getContext";

module Util = {
  external uint8ClampedToArrayInt :
    Js.Typed_array.Uint8ClampedArray.t => array(int) =
    "%identity";

  let initImageData =
      (
        ctx: Ctx.t,
        ~w: int,
        ~h: int,
        ~f: (~x: int, ~y: int) => (int, int, int, int),
      )
      : imageData => {
    let imageData = Ctx.createImageData(ctx, w, h);
    let rawData = uint8ClampedToArrayInt(dataGet(imageData));
    for (y in 0 to h - 1) {
      for (x in 0 to w - 1) {
        let offset = y * (w * 4) + x * 4;
        let (r, g, b, a) = f(x, y);
        rawData[offset] = r;
        rawData[offset + 1] = g;
        rawData[offset + 2] = b;
        rawData[offset + 3] = a;
      };
    };

    imageData;
  };
};

let component = ReasonReact.statelessComponent(__MODULE__);

let make = (~setRef, ~width, ~height, _children) => {
  ...component,
  render: self =>
    <canvas
      ref=setRef
      width=(Js.Int.toString(width))
      height=(Js.Int.toString(height))
    />,
};
