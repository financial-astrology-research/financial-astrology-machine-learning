from optparse import OptionParser

planets = (32768, 16384, 8192, 4096, 2048, 1024, 512, 256, 128, 64, 32)
planets_names = ('SU', 'MO', 'ME', 'VE', 'MA', 'JU', 'SA', 'UR', 'NE', 'PL', 'NN', 'NS')

parser = OptionParser()
(options, args) = parser.parse_args()
numbers = args[0].split('/')
join_number = 0

for number in numbers:
    join_number = join_number | int(number)

i = 0
active_planets = ''

for planet in planets:
    if (join_number & planet):
        active_planets = active_planets + ' ' + planets_names[i]
    i = i+1

print(active_planets)
