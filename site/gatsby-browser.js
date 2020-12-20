exports.onClientEntry = () => {
  if (window.location.pathname === '/') {
    window.location.pathname = `/en`;
  }
};
