# hw-effects

|  Выдано  |     Дедлайн     |
|:--------:|:---------------:|
| 19.02.23 | 23:59, 12.03.23 |

Второе домашнее задание по курсу; создано, чтобы выполняющие его

* поработали с трансформерами монад из transformers/mtl;
* познакомились с паттерном tagless final;
* отработали применение паттернов Handle и RIO.

## Описание проекта

В этом домашнем задании мы будем писать эмулятор банковской системы. Основные
функции:

* Администрирование аккаунтов
* Переводы между аккаунтами

## Описание программы

Программа должна принимать на вход последовательность команд и сразу их
выполнять. С точки зрения пользователя в памяти поддерживается коллекция
аккаунтов и их балансов, а пользователь может:

* Создать аккаунт с начальным балансом в 100 у.е.
* Удалить аккаунт
* Перевести деньги между аккаунтами

После выполнения каждой команды нужно выводить список аккаунтов вместе с
суммами, лежащими на счетах.

Формат ввода-вывода произвольный.

## Шортлист команд

* `cabal build` &mdash; сборка проекта;
* `cabal run hw-effects` &mdash; запуск программы;
* `cabal test` &mdash; запуск тестов;
* `cabal haddock` &mdash; генерация документации по коду в `src/`.

## Чеклист

Сделанные задачи можно помечать с помощью крестика: `- [x]`

- [+] Вписать себя автором и мейнтейнером в `hw-effects.cabal` (0 баллов)
- [+] Сформулировать законы `MonadBank` в `src/Bank.hs` (1.5 балла)
- [+] Исправить ошибку в реализации `transfer` (0.5 балла)
- [+] Перечислить возможные ошибки в `src/Bank/Error.hs` (1 балл)
- [+] Реализовать чистые функции в `pure-bank/Data/Accounts.hs` (1 балл)
- [+] Исправить тип `PureBank` в `pure-bank/Bank/Pure.hs` (0.5 балла)
- [+] Сделать `PureBank` монадой (0.5 балла)
- [ ] Выписать инстанс `MonadBank` для `PureBank` (1 балл)
- [ ] Проверить законы `MonadBank` на `PureBank` в `test/Spec.hs` (1.5 балла)
- [ ] Реализовать интерфейс `BankHandle` в `app/Bank/Handle.hs` (1.5 балла)
- [ ] Реализовать вспомогательные функции в `app/Main.hs` (0.5 балла)
- [ ] Реализовать взаимодействие с пользователем (1 балл)
- [ ] Предложить улучшения интерфейса `MonadBank` в `SUGGESTION.md` (1.5 балла)
