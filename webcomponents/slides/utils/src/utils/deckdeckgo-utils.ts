export function debounce(func: Function, timeout?: number) {
    let timer: number;
    return ($args: any) => {
        if (timer) {
            clearTimeout(timer);
        }

        // @ts-ignore
        timer = setTimeout(func, timeout > 0 ? timeout : 300, $args);
    };
}
