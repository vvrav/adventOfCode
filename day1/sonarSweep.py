

def sonarSweep():
  count = 0

  with open('input') as f:
    buff = f.readline()
    previous = int(buff)
    while len(buff) > 0:
      current = int(buff)
      if current > previous:
          count = count + 1
      previous = current
      buff = f.readline()
  return count


def partTwo():
    count = 0

    with open('input') as f:
        file = []
        buff = f.readline()

        while len(file) < 3:
            file.append(int(buff))
            buff = f.readline()

        previous = sum(file)

        while len(buff) > 0:
            file.pop(0)
            file.append(int(buff))
            current = sum(file)
            if(current > previous):
                count = count + 1
            previous = current
            buff = f.readline()

    return count

