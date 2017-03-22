val calculate :
  sample_rate:int ->
  duration:float option ->
  tempo:float option ->
  sixteenths:int option ->
  (int, string) Result.result
