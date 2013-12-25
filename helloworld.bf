# Brainfuck commands:
# > increment the data pointer
# < decreament the data pointer
# + increment the byte in the data pointer
# - decrement the byte in the data pointer
# . output the byte at the data pointer
# , accept one byte of input, storing its value in the byte at the data pointer
# [ if the byte at the data pointer is zero, 
#   jump to the command after the correspoinding ]
# [ if the byte at the data pointer is nonzero, 
#   jump to the command after the correspoingding [

# I add symbol #, used as comment notation, in my interpreter.
# In order to get a more readable code, I also allow '\n' and 'space'.

+++++ +++++
[                       #loop 10 times

    > +++++ ++          # 70(70)
    > +++++ +++++ +     # 110(117)
    > +++++ ++++        # 90(99)
    > +++++ +++++       # 100((107)
    > +++               # 30(32)

    > +++++ +++++ ++    # 120(121)
    > +++++ +++++ +     # 110(111)
    > +++++ +++++ +     # 110(117)
    > +++++ +++++ +     # 110(114)
    > +++               # 30(32)

    > +++++ ++++        # 90(98)
    > +++++ +++++ +     # 110(114)
    > +++++ ++++        # 90(97)
    > +++++ +++++       # 100(105)
    > +++++ +++++ +     # 110(110)

    <<<<< <<<<< <<<<< -    
]

>   .                   # print 'F'
>   +++++ ++ .          # print 'u'
>   +++++ ++++ .        # print 'c'
>   +++++ ++ .          # print 'k'
>   ++ .                # print ' '

>   + .                 # print 'y'
>   + .                 # print 'o'
>   +++++ ++ .          # print 'u'
>   ++++ .              # print 'r'
>   ++ .                # print ' '

>   +++++ +++ .         # print 'b'
>   ++++ .              # print 'r'
>   +++++ ++ .          # print 'a'
>   +++++ .             # print 'i'
>   .                   # print 'n'
