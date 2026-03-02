export const unsafeParseJSON = (str) => JSON.parse(str);

export const stripKeys = (keys) => (obj) => {
  const result = { ...obj };
  for (const key of keys) {
    delete result[key];
  }
  return result;
};

export const setOperationId = (name) => (jsonStr) => {
  const obj = JSON.parse(jsonStr);
  obj.operationId = name;
  return JSON.stringify(obj);
};
