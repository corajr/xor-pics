open Canvas;
open Canvas.Util;

module type ProgrammaticImage = {
  type input;
  let f: (input, int, int, ~x: int, ~y: int) => (int, int, int, int);
};

module Make = (ProgrammaticImage: ProgrammaticImage) => {
  include ProgrammaticImage;

  let draw = (input, width, height, theRef) =>
    switch (Js.Nullable.toOption(theRef)) {
    | Some(canvas) =>
      let ctx = getContext(canvas);
      let imageData =
        initImageData(ctx, ~w=width, ~h=height, ~f=f(input, width, height));
      Ctx.putImageData(ctx, ~imageData, ~dx=0, ~dy=0);
    | None => ()
    };

  let component = ReasonReact.statelessComponent("ProgrammaticImage");

  let make = (~setRef, ~width, ~height, ~input: input, _children) => {
    ...component,
    render: _self =>
      <Canvas
        setRef=(
          theRef => {
            draw(input, width, height, theRef);
            setRef(theRef);
          }
        )
        width
        height
      />,
  };
};

module Img = {
  [@bs.set] external onload : (Dom.element, unit => unit) => unit = "";

  let component = ReasonReact.statelessComponent("Img");
  let make = (~setRef, ~src, ~width, ~height, _children) => {
    ...component,
    render: _self =>
      <img
        ref=(
          theRef =>
            switch (Js.Nullable.toOption(theRef)) {
            | Some(image) => onload(image, () => setRef(theRef))
            | None => ()
            }
        )
        src
        width=(Js.Int.toString(width))
        height=(Js.Int.toString(height))
      />,
  };
};

module StaticCanvas = {
  let component = ReasonReact.statelessComponent("StaticCanvas");
  let make = (~setRef, ~draw, ~width, ~height, _children) => {
    ...component,
    render: _self =>
      <canvas
        ref=(
          theRef =>
            switch (Js.Nullable.toOption(theRef)) {
            | Some(canvas) =>
              draw(width, height, canvas);
              setRef(theRef);
            | None => ()
            }
        )
        width=(Js.Int.toString(width))
        height=(Js.Int.toString(height))
      />,
  };
};

module Fill =
  Make({
    type input = (int, int, int);
    let f = ((r, g, b), _width, _height, ~x, ~y) => (r, g, b, 255);
  });

module XorTexture =
  Make({
    type input = unit;
    let f = (_input, _width, _height, ~x, ~y) => {
      let v = x lxor y;
      (v, v, v, 255);
    };
  });

module XorFractal =
  Make({
    type input = unit;

    let on = 64;
    let off = 192;

    let f = (_input, _width, _height, ~x, ~y) => {
      let v = x lxor y mod 3;
      switch (v) {
      | 0 => (on, off, off, 255)
      | 1 => (off, on, off, 255)
      | 2
      | _ => (off, off, on, 255)
      };
    };
  });

type circleDef = {
  radius: int,
  color: (int, int, int),
};

module Circle =
  Make({
    type input = circleDef;
    let f = ({radius, color: (r, g, b)}, width, height, ~x, ~y) => {
      let centerX = width / 2;
      let centerY = height / 2;
      let radiusSquared = radius * radius;
      let (cX, cY) = (x - centerX, y - centerY);
      let inCircle = abs(cX * cX + cY * cY - radiusSquared) < radius;
      inCircle ? (r, g, b, 255) : (0, 0, 0, 255);
    };
  });

module RGGamut =
  Make({
    type input = int;
    let f = (input, _width, _height, ~x, ~y) => (x, y, input, 255);
  });

module RBGamut =
  Make({
    type input = int;
    let f = (input, _width, _height, ~x, ~y) => (x, input, y, 255);
  });

module GBGamut =
  Make({
    type input = int;
    let f = (input, _width, _height, ~x, ~y) => (input, x, y, 255);
  });

module PseudoRandom =
  Make({
    type input = int;

    let f = (input, _width, _height, ~x, ~y) => {
      if (x == 0 && y == 0) {
        Random.init(input);
      };
      (Random.int(256), Random.int(256), Random.int(256), 255);
    };
  });

module PseudoRandomGray =
  Make({
    type input = int;

    let f = (input, _width, _height, ~x, ~y) => {
      if (x == 0 && y == 0) {
        Random.init(input);
      };
      let v = Random.int(256);
      (v, v, v, 255);
    };
  });
