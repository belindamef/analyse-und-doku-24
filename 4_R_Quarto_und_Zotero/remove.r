

if (1 == 1) {
  print("True")
}

for (i in 1:3) {
  print(i)                         # Aktion, die wiederholt werden soll
}


i <- 5
while (i < 11) {
  print(i)
  i <- i + 1
}

i <- 1
repeat {
  print(i)
  i <- i + 1
  if (i == 5) {
    break
  }
}