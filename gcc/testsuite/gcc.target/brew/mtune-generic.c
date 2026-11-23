/* -mtune=generic is the default and defines no tuning macro.  */
/* { dg-do preprocess } */

#ifdef __BREW_TUNE_ESPRESSO__
#error __BREW_TUNE_ESPRESSO__ defined without -mtune=espresso
#endif
