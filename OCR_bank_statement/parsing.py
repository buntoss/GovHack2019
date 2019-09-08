# pseudocode

For each transaction
    Use regex/glob to find the previous and current balance
    Compare the previous balance with the current balance to determine if the current transaction is a debit or credit
    if the difference is positive
        Debit
    Else
        Credit