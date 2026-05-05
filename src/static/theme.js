// mini gestion de thème (optionnel)
(() => {
  const key = 'theme';
  const root = document.documentElement;
  const saved = localStorage.getItem(key);
  if (saved) root.dataset.theme = saved;
  window.toggleTheme = () => {
    const next = root.dataset.theme === 'dark' ? 'light' : 'dark';
    root.dataset.theme = next;
    localStorage.setItem(key, next);
  };
})();
