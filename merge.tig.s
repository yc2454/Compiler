sw $zero, 0($fp)
L354:
sw $zero, 0($fp)
addi $zero, $fp, 36
move $zero, $zero
sw $zero, 0($fp)
jal getchar
move $zero, $zero
sw $zero, 0($fp)
sw $zero, 0($zero)
sw $zero, 0($fp)
addi $zero, $fp, 52
move $zero, $zero
sw $zero, 0($fp)
lw $zero, 0($fp)
lw t559, 0($zero)
move $zero, t559
jal L278
move $zero, $zero
sw $zero, 0($fp)
sw $zero, 0($zero)
sw $zero, 0($fp)
addi $zero, $fp, 56
move $zero, $zero
sw $zero, 0($fp)
lw $zero, 0($fp)
lw t562, 0($zero)
move $zero, t562
jal L278
move $zero, $zero
sw $zero, 0($fp)
sw $zero, 0($zero)
sw $zero, 0($fp)
lw $zero, 0($fp)
lw t556, 0($zero)
sw $zero, 0($fp)
lw $zero, 0($fp)
lw t565, 0($zero)
move $zero, t565
lw t567, 52($fp)
move $zero, t567
lw t568, 56($fp)
move $zero, t568
jal L279
move t555, $zero
sw $zero, 0($fp)
move $zero, t556
move $zero, t555
jal L281
sw $zero, 0($fp)
li $zero, 0
sw $zero, 0($fp)
j L353
sw $zero, 0($fp)
L353:
j return
