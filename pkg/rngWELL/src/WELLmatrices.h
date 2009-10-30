//Mi matrices defined in table 1 of Panneton et al (2006)
//matrix M3(t)
#define MAT0POS(t,v) (v^(v>>t))
#define MAT0NEG(t,v) (v^(v<<(-(t))))
//matrix M1
#define MAT1(v) v
//matrix M4(a)
#define MAT2(a,v) ((v & 1U)?((v>>1)^a):(v>>1))
//matrix M2(t)
#define MAT3POS(t,v) (v>>t)
#define MAT3NEG(t,v) (v<<(-(t)))
//matrix M5(t,b)
#define MAT4POS(t,b,v) (v ^ ((v>>  t ) & b))
#define MAT4NEG(t,b,v) (v ^ ((v<<(-(t))) & b))
//matrix M6(q,s,t,a)
#define MAT5(r,a,ds,dt,v) ((v & dt)?((((v<<r)^(v>>(W-r)))&ds)^a):(((v<<r)^(v>>(W-r)))&ds))
//matrix M0
#define MAT7(v) 0

