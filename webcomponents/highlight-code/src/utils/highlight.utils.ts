export const attachHighlightObserver = ({
  refContainer,
  refCode,
  highlightLines,
}: {
  refContainer: HTMLElement;
  refCode: HTMLElement;
  highlightLines: string | undefined;
}) => {
  if (!highlightLines || highlightLines.length <= 0) {
    return;
  }

  if (window && 'ResizeObserver' in window) {
    // @ts-ignore
    const observer: ResizeObserver = new ResizeObserver(async (_entries) => {
      await addHighlight({refCode, highlightLines});

      observer.disconnect();
    });

    observer.observe(refContainer);
  } else {
    // Back in my days...
    setTimeout(async () => {
      await addHighlight({refCode, highlightLines});
    }, 100);
  }
};

const addHighlight = async ({highlightLines, refCode}: {highlightLines: string | undefined; refCode: HTMLElement}): Promise<void> => {
  if (!refCode.hasChildNodes()) {
    return;
  }

  const {rows, rowsGroup}: {rows: number[]; rowsGroup: Record<string, number>} = await findRowsToHighlight({highlightLines});

  if (rows.length <= 0) {
    return;
  }

  let rowIndex: number = 0;
  let lastOffsetTop: number = -1;
  let offsetHeight: number = -1;

  Array.from(refCode.childNodes).forEach((element: HTMLElement) => {
    // We try to find the row index with the offset of the element
    rowIndex = element.offsetTop > lastOffsetTop ? rowIndex + 1 : rowIndex;
    lastOffsetTop = element.offsetTop;

    // For some reason, some converted text element are displayed on two lines, that's why we should consider the 2nd one as index
    offsetHeight = offsetHeight === -1 || offsetHeight > element.offsetHeight ? element.offsetHeight : offsetHeight;

    const rowsIndexToCompare: number = element.offsetHeight > offsetHeight ? rowIndex + 1 : rowIndex;

    if (rows.indexOf(rowsIndexToCompare) > -1) {
      element.classList.add('highlight', `group-${rowsGroup[`row_${rowsIndexToCompare}`]}`);
    }
  });

  refCode.classList.add('animate');
};

const findRowsToHighlight = async ({highlightLines}: {highlightLines: string | undefined}): Promise<{rows: number[]; rowsGroup: Record<string, number>}> => {
  const groups: string[] = highlightLines.split(' ');

  if (!groups || groups.length <= 0) {
    return {
      rows: [],
      rowsGroup: {},
    };
  }

  const rows: number[] = [];
  let rowsGroup: Record<string, number> = {};

  groups.forEach((group: string, groupIndex: number) => {
    const index: string[] = group.replace(/-/g, ',').split(',');

    if (index && index.length >= 1) {
      const start: number = parseInt(index[0], 0);
      const end: number = parseInt(index[1], 0);

      for (let i = start; i <= (isNaN(end) ? start : end); i++) {
        rows.push(i);

        if (!(`row_${i}` in rowsGroup) || rowsGroup[`row_${i}`] > groupIndex) {
          rowsGroup[`row_${i}`] = groupIndex;
        }
      }
    }
  });

  return {
    rows,
    rowsGroup,
  };
};
