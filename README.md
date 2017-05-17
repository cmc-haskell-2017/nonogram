Головоломка "Японский кроссворд".

## Сборка и запуск

Соберите проект при помощи [утилиты Stack](https://www.haskellstack.org):

```
stack setup
stack build
```

Собрать и запустить проект можно при помощи команды

```
stack build && stack exec nonogram
```

Запустить тесты можно при помощи команды

```
stack test
```

Чтобы запустить интепретатор GHCi и автоматически подгрузить все модули проекта, используйте команду

```
stack ghci
```
Для запуска автоматического решателя, воспользуйтесь кнопкой "Autosolver", либо пошагово его можно вызывать с помощью клавиши "Пробел"

![alt text](https://raw.githubusercontent.com/cmc-haskell-2017/nonogram/master/illustration.gif)
