let x = 1;

{
  let y = "Hello Y!";

  x = x + 1;

  log("Inner X = ", x);
  log("Inner Y = ", y);
}

log("Outer X = ", x);
log("Outer Y = ", y);
