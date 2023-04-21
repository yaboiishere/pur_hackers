-module(purHackers_primeTime@foreign).

-export([isPrime/1]).

isPrime(N) ->
  'Elixir.PurHackers.PrimeTime.Prime':'isPrime'(N).
