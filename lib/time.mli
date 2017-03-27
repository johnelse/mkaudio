val calculate_samples :
  sample_rate:int ->
  duration:float option ->
  tempo:float option ->
  steps:int option ->
  (int, string) Result.result
