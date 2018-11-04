let mask = 7;

let circShiftL = (value: int, count: int) => {
  let count = count land mask;
  value lsl count land 0xff lor (value lsr (- count land mask) land 0xff);
};

let circShiftR = (value: int, count: int) => {
  let count = count land mask;
  value lsr count land 0xff lor (value lsl (- count land mask) land 0xff);
};
