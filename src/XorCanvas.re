open Canvas;
open Canvas.Util;
open ImageGenerator;

let logo: string = [%bs.raw {|require('./logo.svg')|}];

type state = {
  canvasRef: ref(option(Dom.element)),
  images: ref(Belt.Map.String.t(Dom.element)),
};

let addToMap = (key, map, theRef) =>
  switch (Js.Nullable.toOption(theRef)) {
  | Some(el) => map := Belt.Map.String.set(map^, key, el)
  | None => ()
  };

let setCanvasRef = (width, height, theRef, {ReasonReact.state}) => {
  state.canvasRef := Js.Nullable.toOption(theRef);
  addToMap("self", state.images, theRef);
  switch (state.canvasRef^) {
  | Some(canvas) =>
    let ctx = getContextWithAttrs(canvas, {"alpha": false});
    Ctx.setImageSmoothing(ctx, false);
    Ctx.setFillStyle(ctx, "black");
    Ctx.fillRect(ctx, 0, 0, width, height);
  | None => ()
  };
};

let draw = (width, height, imageKey, {ReasonReact.state}) =>
  switch (state.canvasRef^, Belt.Map.String.get(state.images^, imageKey)) {
  | (Some(canvas), Some(src)) =>
    Js.log("redrawing");
    let ctx = getContext(canvas);

    let imageData =
      Ctx.getImageData(ctx, ~sx=0, ~sy=0, ~sw=width, ~sh=height);

    let rawOld = uint8ClampedToArrayInt(dataGet(imageData));

    let n = Js.Typed_array.Uint8ClampedArray.length(dataGet(imageData)) / 4;

    Ctx.drawImageDestRect(
      ctx,
      ~image=src,
      ~dx=0,
      ~dy=0,
      ~dw=width,
      ~dh=height,
    );
    let newImageData =
      Ctx.getImageData(ctx, ~sx=0, ~sy=0, ~sw=width, ~sh=height);

    let rawNew = uint8ClampedToArrayInt(dataGet(newImageData));
    for (i in 0 to n - 1) {
      let offset = i * 4;
      rawOld[offset] = rawOld[offset] lxor rawNew[offset];
      rawOld[offset + 1] = rawOld[offset + 1] lxor rawNew[offset + 1];
      rawOld[offset + 2] = rawOld[offset + 2] lxor rawNew[offset + 2];
      rawOld[offset + 3] = max(rawOld[offset + 3], rawNew[offset + 3]);
    };

    Ctx.putImageData(ctx, ~imageData, ~dx=0, ~dy=0);
  | _ => ()
  };

module Clickable = {
  let component = ReasonReact.statelessComponent("Clickable");

  let make = (~render, ~name, ~onClick, _children) => {
    ...component,
    render: _self =>
      <div onClick=(_evt => onClick(name))> (render(name)) </div>,
  };
};

let component = ReasonReact.reducerComponent(__MODULE__);

let make = (~width=256, ~height=256, _children) => {
  ...component,
  initialState: () => {
    canvasRef: ref(None),
    images: ref(Belt.Map.String.empty),
  },
  reducer: ((), _state) => ReasonReact.NoUpdate,
  render: self => {
    let onClick = name => draw(width, height, name, self);
    let fillConstantColor = 191;
    let gamutConstantColor = 191;

    <div>
      <div style=(ReactDOMRe.Style.make(~position="fixed", ()))>
        <Clickable
          name="self"
          onClick
          render=(
            name =>
              <canvas
                ref=(self.handle(setCanvasRef(width, height)))
                style=(ReactDOMRe.Style.make(~border="1px solid black", ()))
                width=(Js.Int.toString(width))
                height=(Js.Int.toString(height))
              />
          )
        />
      </div>
      <div style=(ReactDOMRe.Style.make(~marginLeft="384px", ()))>
        <div
          style=(
            ReactDOMRe.Style.make(
              ~display="flex",
              ~flexDirection="row",
              ~flexWrap="wrap",
              ~width="100vw",
              ~transform="scale(0.5)",
              ~transformOrigin="top left",
              (),
            )
          )>
          <Clickable
            name="logo"
            onClick
            render=(
              name =>
                <Img
                  setRef=(addToMap(name, self.state.images))
                  src="images/xorpics.png"
                  width
                  height
                />
            )
          />
          <Clickable
            name="gray-25"
            onClick
            render=(
              name =>
                <Fill
                  setRef=(addToMap(name, self.state.images))
                  input=(63, 63, 63)
                  width
                  height
                />
            )
          />
          <Clickable
            name="gray-50"
            onClick
            render=(
              name =>
                <Fill
                  setRef=(addToMap(name, self.state.images))
                  input=(127, 127, 127)
                  width
                  height
                />
            )
          />
          <Clickable
            name="gray-75"
            onClick
            render=(
              name =>
                <Fill
                  setRef=(addToMap(name, self.state.images))
                  input=(191, 191, 191)
                  width
                  height
                />
            )
          />
          <Clickable
            name="white"
            onClick
            render=(
              name =>
                <Fill
                  setRef=(addToMap(name, self.state.images))
                  input=(255, 255, 255)
                  width
                  height
                />
            )
          />
          <Clickable
            name="white-circle"
            onClick
            render=(
              name => {
                let input = {radius: 20, color: (255, 255, 255)};

                <Circle
                  setRef=(addToMap(name, self.state.images))
                  input
                  width
                  height
                />;
              }
            )
          />
          <Clickable
            name="xor-texture"
            onClick
            render=(
              name =>
                <XorTexture
                  setRef=(addToMap(name, self.state.images))
                  input=()
                  width
                  height
                />
            )
          />
          <Clickable
            name="red"
            onClick
            render=(
              name =>
                <Fill
                  setRef=(addToMap(name, self.state.images))
                  input=(fillConstantColor, 0, 0)
                  width
                  height
                />
            )
          />
          <Clickable
            name="green"
            onClick
            render=(
              name =>
                <Fill
                  setRef=(addToMap(name, self.state.images))
                  input=(0, fillConstantColor, 0)
                  width
                  height
                />
            )
          />
          <Clickable
            name="blue"
            onClick
            render=(
              name =>
                <Fill
                  setRef=(addToMap(name, self.state.images))
                  input=(0, 0, fillConstantColor)
                  width
                  height
                />
            )
          />
          <Clickable
            name="gb"
            onClick
            render=(
              name =>
                <GBGamut
                  setRef=(addToMap(name, self.state.images))
                  input=gamutConstantColor
                  width
                  height
                />
            )
          />
          <Clickable
            name="rb"
            onClick
            render=(
              name =>
                <RBGamut
                  setRef=(addToMap(name, self.state.images))
                  input=gamutConstantColor
                  width
                  height
                />
            )
          />
          <Clickable
            name="rg"
            onClick
            render=(
              name =>
                <RGGamut
                  setRef=(addToMap(name, self.state.images))
                  input=gamutConstantColor
                  width
                  height
                />
            )
          />
          <Clickable
            name="xor-fractal"
            onClick
            render=(
              name =>
                <XorFractal
                  setRef=(addToMap(name, self.state.images))
                  input=()
                  width
                  height
                />
            )
          />
          <Clickable
            name="floral"
            onClick
            render=(
              name =>
                <Img
                  setRef=(addToMap(name, self.state.images))
                  src="images/floral_shoppe.jpg"
                  width
                  height
                />
            )
          />
          <Clickable
            name="cats"
            onClick
            render=(
              name =>
                <Img
                  setRef=(addToMap(name, self.state.images))
                  src="images/koko_and_winnie.jpg"
                  width
                  height
                />
            )
          />
          <Clickable
            name="mandrill"
            onClick
            render=(
              name =>
                <Img
                  setRef=(addToMap(name, self.state.images))
                  src="images/mandrill.png"
                  width
                  height
                />
            )
          />
          <Clickable
            name="random-rgb"
            onClick
            render=(
              name =>
                <PseudoRandom
                  setRef=(addToMap(name, self.state.images))
                  input=0
                  width
                  height
                />
            )
          />
          <Clickable
            name="cats-concealed"
            onClick
            render=(
              name =>
                <Img
                  setRef=(addToMap(name, self.state.images))
                  src="images/koko_and_winnie_hidden.png"
                  width
                  height
                />
            )
          />
          <Clickable
            name="ich_ruf_zu_dir"
            onClick
            render=(
              name =>
                <Img
                  setRef=(addToMap(name, self.state.images))
                  src="images/ich_ruf_zu_dir.png"
                  width
                  height
                />
            )
          />
        </div>
      </div>
    </div>;
  },
};
