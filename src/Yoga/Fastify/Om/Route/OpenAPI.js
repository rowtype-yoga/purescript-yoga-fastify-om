export const unsafeParseJSON = (str) => JSON.parse(str);

export const stripKeys = (keys) => (obj) => {
  const result = { ...obj };
  for (const key of keys) {
    delete result[key];
  }
  return result;
};
