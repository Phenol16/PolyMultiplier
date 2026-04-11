# TOOM-COOK 多项式乘法

$$\left[ \begin{matrix} \mathbb{A}(\infin) \\ \mathbb{A}(2) \\ \mathbb{A}(1) \\ \mathbb{A}(-1) \\ 8\mathbb{A}(1/2) \\ 8\mathbb{A}(-1/2) \\ \mathbb{A}(0)  \end{matrix} \right] = 
\left[ \begin{matrix} 0 & 0 & 0 & 1 \\ 1 & 2 & 4 & 8 \\ 1 & 1 & 1 & 1 \\ 1 & -1 & 1 & -1 \\ 8 & 4 & 2 & 1 \\ 8 & -4 & 2 & -1 \\ 1 & 0 & 0 & 0 \end{matrix} \right]
\left[ \begin{matrix} A_0 \\ A_1 \\ A_2 \\ A_3 \end{matrix} \right]$$

$$\left[ \begin{matrix} \mathbb{B}(\infin) \\ \mathbb{B}(2) \\ \mathbb{B}(1) \\ \mathbb{B}(-1) \\ 8\mathbb{B}(1/2) \\ 8\mathbb{B}(-1/2) \\ \mathbb{B}(0)  \end{matrix} \right] = 
\left[ \begin{matrix} 0 & 0 & 0 & 1 \\ 1 & 2 & 4 & 8 \\ 1 & 1 & 1 & 1 \\ 1 & -1 & 1 & -1 \\ 8 & 4 & 2 & 1 \\ 8 & -4 & 2 & -1 \\ 1 & 0 & 0 & 0 \end{matrix} \right]
\left[ \begin{matrix} B_0 \\ B_1 \\ B_2 \\ B_3 \end{matrix} \right]$$

$$\left[ \begin{matrix} \mathbb{C}(\infin) \\ \mathbb{C}(2) \\ \mathbb{C}(1) \\ \mathbb{C}(-1) \\ 64\mathbb{C}(1/2) \\ 64\mathbb{C}(-1/2) \\ \mathbb{C}(0)  \end{matrix} \right] = 
\left[ \begin{matrix} \mathbb{A}(\infin) \times \mathbb{B}(\infin) \\ \mathbb{A}(2) \times \mathbb{B}(2) \\ \mathbb{A}(1) \times \mathbb{B}(1) \\ \mathbb{A}(-1) \times \mathbb{B}(-1) \\ 8\mathbb{A}(1/2) \times 8\mathbb{B}(1/2) \\ 8\mathbb{A}(-1/2) \times 8\mathbb{B}(-1/2) \\ \mathbb{A}(0) \times \mathbb{B}(0)  \end{matrix} \right] = 
\left[ \begin{matrix} 0 & 0 & 0 & 0 & 0 & 0 & 1 \\ 1 & 2 & 4 & 8 & 16 & 32 & 64 \\ 1 & 1 & 1 & 1 & 1 & 1 & 1 \\ 1 & -1 & 1 & -1 & 1 & -1 & 1 \\ 64 & 32 & 16 & 8 & 4 & 2 & 1 \\ 64 & -32 & 16 & -8 & 4 & -2 & 1 \\ 1 & 0 & 0 & 0 & 0 & 0 & 0 \end{matrix} \right]
\left[ \begin{matrix} C_0 \\ C_1 \\ C_2 \\ C_3 \\ C_4 \\ C_5 \\ C_6 \end{matrix} \right]$$

$$ \mathbb{A}(\infin) \times \mathbb{B}(\infin) = A_3B_3 $$

$$ \mathbb{A}(2) \times \mathbb{B}(2) = A_0B_0 + 2(A_0B_1 + A_1B_0) + 4(A_0B_2 + A_1B_1 + A_2B_0) + 8(A_0B_3 + A_1B_2 + A_2B_1 + A_3B_0) + 16(A_1B_3 + A_2B_2 + A_3B_1) + 32(A_2B_3 + A_3B_2) + 64A_3B_3 $$

$$ \mathbb{A}(1) \times \mathbb{B}(1) = A_0B_0 + (A_0B_1 + A_1B_0) + (A_0B_2 + A_1B_1 + A_2B_0) + (A_0B_3 + A_1B_2 + A_2B_1 + A_3B_0) + (A_1B_3 + A_2B_2 + A_3B_1) + (A_2B_3 + A_3B_2) + A_3B_3 $$

$$ \mathbb{A}(-1) \times \mathbb{B}(-1) = A_0B_0 - (A_0B_1 + A_1B_0) + (A_0B_2 + A_1B_1 + A_2B_0) - (A_0B_3 + A_1B_2 + A_2B_1 + A_3B_0) + (A_1B_3 + A_2B_2 + A_3B_1) - (A_2B_3 + A_3B_2) + A_3B_3 $$

$$ 8\mathbb{A}(1/2) \times 8\mathbb{B}(1/2) = 64A_0B_0 + 32(A_0B_1 + A_1B_0) + 16(A_0B_2 + A_1B_1 + A_2B_0) + 8(A_0B_3 + A_1B_2 + A_2B_1 + A_3B_0) + 4(A_1B_3 + A_2B_2 + A_3B_1) + 2(A_2B_3 + A_3B_2) + A_3B_3 $$

$$ 8\mathbb{A}(1/2) \times 8\mathbb{B}(1/2) = 64A_0B_0 - 32(A_0B_1 + A_1B_0) + 16(A_0B_2 + A_1B_1 + A_2B_0) - 8(A_0B_3 + A_1B_2 + A_2B_1 + A_3B_0) + 4(A_1B_3 + A_2B_2 + A_3B_1) - 2(A_2B_3 + A_3B_2) + A_3B_3 $$

$$ \mathbb{A}(0) \times \mathbb{B}(0) = A_0B_0 $$

$$\left[ \begin{matrix} C_0 \\ C_1 \\ C_2 \\ C_3 \\ C_4 \\ C_5 \\ C_6  \end{matrix} \right] = 
\left[ \begin{matrix} 0 & 0 & 0 & 0 & 0 & 0 & 1 \\ -1/2 & 1/90 & -1/3 & 1/9 & 1/36 & -1/60 & -1/2 \\ 1/4 & 0 & -1/6 & -1/6 & 1/24 & 1/24 & -5 \\ 5/2 & -1/18 & 3/2 & -7/18 & -1/18 & 0 & 5/2 \\ -5/4 & 0 & 2/3 & 2/3 & -1/24 & -1/24 & 4 \\ -2 & 2/45 & -2/3 & -2/9 & 1/36 & 1/60 & -2 \\ 1 & 0 & 0 & 0 & 0 & 0 & 0 \end{matrix} \right]
\left[ \begin{matrix} \mathbb{C}(\infin) \\ \mathbb{C}(2) \\ \mathbb{C}(1) \\ \mathbb{C}(-1) \\ 64\mathbb{C}(1/2) \\ 64\mathbb{C}(-1/2) \\ \mathbb{C}(0)  \end{matrix} \right] = 
\left[ \begin{matrix} 0 & 0 & 0 & 0 & 0 & 0 & 1 \\ -90/180 & 2/180 & -60/180 & 20/180 & 5/180 & -3/180 & -90/180 \\ 6/24 & 0 & -4/24 & -4/24 & 1/24 & 1/24 & -120/24 \\ 45/18 & -1/18 & 27/18 & -7/18 & -1/18 & 0 & 45/18 \\ -30/24 & 0 & 16/24 & 16/24 & -1/24 & -1/24 & 96/24 \\ -360/180 & 8/180 & -120/180 & -40/180 & 5/180 & 3/180 & -360/180 \\ 1 & 0 & 0 & 0 & 0 & 0 & 0 \end{matrix} \right]
\left[ \begin{matrix} \mathbb{C}(\infin) \\ \mathbb{C}(2) \\ \mathbb{C}(1) \\ \mathbb{C}(-1) \\ 64\mathbb{C}(1/2) \\ 64\mathbb{C}(-1/2) \\ \mathbb{C}(0)  \end{matrix} \right] $$

