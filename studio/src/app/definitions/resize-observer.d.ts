interface ResizeObserverConstructor {
    new (callback: any);

    observe: (target: HTMLElement) => void;

    unobserve: (target: HTMLElement) => void;

    disconnect: () => void;
}

declare var ResizeObserver: ResizeObserverConstructor;
