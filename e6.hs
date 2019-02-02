-- Sum square difference

main::IO()
main = do
    let sqs = sum[x^2 | x <- [1..100]]
    let ssq = sum [1..100] ^ 2
    print (ssq - sqs)
