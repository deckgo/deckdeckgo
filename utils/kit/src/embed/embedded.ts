EMBEDDED = false;

export const initEmbedded = () => {
  if (!window || !window.location) {
    return;
  }

  const url = new URL(window.location.href);
  EMBEDDED = url && url.searchParams ? url.searchParams.has('embed') : false;
};
