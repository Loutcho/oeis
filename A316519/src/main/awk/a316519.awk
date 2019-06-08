function a(n) {
  r = 0; split("", T)
  for (i = 2; i < n; i ++) if (n % i != 0)
  for (j = i + 1; j < n; j ++) if (n % j != 0)
  if ((n * (j - i)) % (i * j) == 0)
  if (i in T) { T[j] = T[i] } else { T[j] = i }
  for (i = 2; i < n; i ++) if (n % i != 0)
  if (!(i in T)) { r ++ }
  return r
}
BEGIN { for (n = 1; n <= 100; n ++) printf("%d, ", a(n)) }
