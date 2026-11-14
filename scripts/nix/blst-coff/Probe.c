/* Link the unchanged BLST C owner and assembly into one real PE image. */
extern int __blst_platform_cap;

int main(void)
{
    return __blst_platform_cap < 0 ? 1 : 0;
}
