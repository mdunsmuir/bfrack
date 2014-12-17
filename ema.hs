import Control.Monad.State

type EMAState = Double
type Tau = Double

ema :: Tau -> Double -> State EMAState Double
ema tau x = state $ \y ->
  let alpha = 1 / tau
      mu = exp(-alpha)
      mu' = 1 - mu
      y' = (mu * y) + (mu' * x)
  in (y', y')

ema' _ [] = get >>= return
ema' tau (x:xs) = do
  y <- get
  let alpha = 1 / tau
      mu = exp $ negate alpha
      mu' = 1 - mu
      y' = (mu * y) + (mu' * x)
  put y'
  ema' tau xs
