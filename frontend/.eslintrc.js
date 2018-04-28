module.exports = {
  parser: 'babel-eslint',
  env: {
    es6: true,
    browser: true
  },
  plugins: ['react', 'babel'],
  parserOptions: {
    ecmaVersion: 6,
    sourceType: "module",
    ecmaFeatures: {
      modules: true,
      jsx: true,
      experimentalObjectRestSpread: true
    }
  },

  settings: {
    react: {
      version: '16.0'
    }
  },

  rules: {
    // vars started with _ do not trigger warning
    'no-unused-vars': ['warn', {varsIgnorePattern: "^_", argsIgnorePattern: "^_"}],

    'prefer-const': 'warn',
    'prefer-destructuring': ['warn', { object: true }],
    'no-var': 'warn',
    'no-dupe-keys': 'warn',
    'no-duplicate-case': 'warn',
    'use-isnan': 'warn',
    'valid-typeof': 'warn',
    'no-with': 'warn',
    'no-shadow': 'warn',
    'no-const-assign': 'warn',
    'no-dupe-class-members': 'warn',
    'no-duplicate-imports': 'warn',

    'react/display-name': 'warn',
    'react/jsx-uses-vars': 'warn',
    'react/jsx-uses-react': 'warn',
    'react/jsx-pascal-case': 'warn',
    'react/jsx-no-undef': 'warn',
    'react/jsx-no-duplicate-props': 'warn',
    'react/no-will-update-set-state': 'warn',
    'react/no-string-refs': 'warn',
    'react/no-deprecated': 'warn',
  }
}