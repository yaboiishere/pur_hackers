defmodule PurHackers.PrimeTime.Prime do
  def isPrime(2), do: true
  def isPrime(3), do: true
  def isPrime(n) when n < 0, do: false

  def isPrime(n) do
    upper_bound = n |> :math.sqrt() |> trunc()

    Enum.all?(2..upper_bound, fn x -> rem(n, x) != 0 end)
  end
end
