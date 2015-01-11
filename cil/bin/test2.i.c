# 1 "test2.c"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "test2.c"
# 1 "test.h" 1
typedef signed char siint08;
typedef unsigned char unint08;
typedef signed short siint16;
typedef unsigned short unint16;
typedef signed int siint32;
typedef unsigned int unint32;
typedef float float32;
typedef double float64;
typedef unsigned int polymor;
typedef unsigned int dimorph;
typedef unsigned char boolu08;
typedef unsigned short boolu16;
typedef unsigned int boolean32;

extern void B_IntTms(void);
extern void B_IemsTask(void);
extern void B_SendTmsData(void);
extern void B_TmPack(void);
# 2 "test2.c" 2
void main(){
  B_IntTms();
  return;
}
