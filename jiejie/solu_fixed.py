
import csv

g_records = None

def load_csv_file(file_name):
    # how to load csv file check here:
    # https://pythonprogramminglanguage.com/read-csv/
    with open(file_name) as csvfile:  # open a file and get its handler
        csvReader = csv.reader(csvfile, delimiter=';')  # call the csv module
        l = [item for item in csvReader]  # read the file line by line
        return l[1:]  # skip the first line

def solu_2nd():
    d = {}  # create an empty map
    for record in g_records:  # for each record
        sexe, preusuel, annais, nombre = record  # unpacking
        if preusuel == '_PRENOMS_RARES':
            continue  # skip abnormal data
        if preusuel in d:
            d[preusuel] += int(nombre)  # string -> int
        else:  # first occurs, set it current value
            d[preusuel] = int(nombre)

    result = 0
    for preusuel in d.keys():
        if d[preusuel] < 100:
            result += 1
    # the following three lines are used for test
    return result

def solu_3rd(specified_name):
    result = 0
    for record in g_records:  # for each record
        sexe, preusuel, annais, nombre = record  # unpacking
        if annais == 'XXXX':  # skip
            continue
        if preusuel == specified_name and int(annais) >= 1900:
            result += int(nombre)
    return result


def solu_4th(X, Y):
    d = {}  # create an empty map
    for record in g_records:  # for each record
        sexe, preusuel, annais, nombre = record  # unpacking
        if annais == 'XXXX' or preusuel == '_PRENOMS_RARES':
            continue  # skip abnormal data
        if annais != str(Y):
            continue

        if preusuel in d:
            d[preusuel] += int(nombre)
        else:
            d[preusuel] = int(nombre)

    # about sorted, check here:
    # https://stackoverflow.com/questions/613183/how-do-i-sort-a-dictionary-by-value
    sorted_l = sorted(d.items(), key=lambda item: item[1], reverse=True)
    result = [sorted_l[i][0] for i in range(int(X))]
    #print(result)
    return result

def solu_5th():
    last_n_years = 5
    max_year = 2019
    result = []
    for i in range(last_n_years):
        result.append(solu_4th(3, max_year - i))

    # print without newline, check here:
    # https://www.geeksforgeeks.org/print-without-newline-python/
    print('question 5:')
    for i in range(last_n_years):
        current_year = max_year - i;
        name1, name2, name3 = result[i]
        print('{0: <8}{1: <8}    {2: <8}    {3: <8}'.format(current_year, name1, name2, name3))

def test():
    # question 1
    global g_records
    g_records = load_csv_file('nat2019.csv')
    print('file nat2019.csv contains ' , len(g_records) , ' valid lines')

    # question 2
    print('question 2:')
    print(solu_2nd())
    print()

    # question 3
    print('question 3:')
    print(solu_3rd('ZULAL'))  # for test
    print()

    # question 4
    print('question 4:')
    print(solu_4th(10, 2019))  # for test
    print()

    solu_5th()

test()
