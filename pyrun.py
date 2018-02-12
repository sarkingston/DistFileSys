#!/usr/local/bin/python

def main():
  os.system("cd Directory-Service && stack build && stack exec Directory-Service-exe")
  for i in range 3:
    os.system("cd FileServer && stack build && stack exec FileServer-exe 400" + str(i) + " begroup")
  for i in range 3:
    os.system("cd FileServer && stack exec FileServer-exe 500" + str(i) + " bebackup")

if __name__ == "__main__":
  main()
    
