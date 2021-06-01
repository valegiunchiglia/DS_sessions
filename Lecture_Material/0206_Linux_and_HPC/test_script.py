"""

Script to get the first N fibonacci numbers (defaults to 1.000)

"""

import sys

 

print("="*80)

print("-"*30 + " FIBONACCI " + "-"*30)

 

def fibo(n):

  L = [1,2]

  for j in range(n-2):

    L.append(L[-1] + L[-2])

  return L

 

def main():

  if len(sys.argv) > 1:

    N = int(sys.argv[1])

  else:

    N = 1000

   

  print("Getting the first {} fibonacci numbers".format(N))

  fibolist = fibo(N)

  print("Writing output to fibo_numbers.txt...")

 

  with open('fibo_numbers.txt', 'w') as file:

    for i,num in enumerate(fibolist):

      file.write("[{}] {}\n".format(i+1,num))

 

  

  print("-"*35 + " DONE " + "-"*35)

  print("="*80)

 

if __name__ == "__main__":

  main()
