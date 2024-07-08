let word = "Buenos Aires";
let left = 0;
let right = word.length - 1;

while (left < right) {
  if (word[left] !== word[right]) {
    log("false");
    break;
  }
  left = left + 1;
  right = right - 1;
}

log("true");
