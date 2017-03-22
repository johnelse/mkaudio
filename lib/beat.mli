type beat = {
  kick: bool;
  snare: bool;
  hihat: bool;
}

val parse_patterns :
  kick:string option ->
  snare:string option ->
  hihat:string option ->
  (beat list, string) Result.result

