export function findSlidesTitle(): Promise<string[]> {
    return new Promise<string[]>((resolve) => {
        if (!document) {
            resolve();
            return;
        }

        const results: string[] = [];

        const slides: NodeListOf<HTMLElement> = document.querySelectorAll('deckgo-deck > *');

        if (slides) {
            for (const slide of Array.from(slides)) {
                if (slide.tagName && slide.tagName.toLowerCase().indexOf('deckgo-slide') > -1) {
                    const title: HTMLElement | null = slide.querySelector('[slot="title"]');

                    if (title && title.textContent && title.textContent !== '') {
                        results.push(title.textContent);
                    } else {
                        const start: HTMLElement | null = slide.querySelector('[slot="start"],[slot="header"]');

                        if (start && start.textContent && start.textContent !== '') {
                            results.push(start.textContent);
                        } else {
                            const end: HTMLElement | null = slide.querySelector('[slot="end"],[slot="footer"]');

                            if (end && end.textContent && end.textContent !== '') {
                                results.push(end.textContent);
                            } else {
                                results.push('');
                            }
                        }
                    }
                }
            }
        }

        resolve(results);
    });
}
