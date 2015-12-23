#!/bin/env python

'''Upload the student list to parse.com for the Into to R course Socratic Swirl exercises.

See: https://www.parse.com/docs/rest/guide/#objects-creating-objects
'''

course1keys = {'test' : {'app' : '<parse.com application ID>',
                         'api' : '<parse.com API key>',
                         'master' : '<parse.com master key>'},
               'prod' : {'app' : '<parse.com application ID>',
                         'api' : '<parse.com API key>',
                         'master' : '<parse.com API key>'}}
    
course2keys = {'test' : {'app' : '<parse.com application ID>',
                         'api' : '<parse.com API key>',
                         'master' : '<parse.com API key>'},
               'prod' : {'app' : '<parse.com application ID>',
                         'api' : '<parse.com API key>',
                         'master' : '<parse.com API key>'}}

keys = {'course1' : course1keys,
        'course2' : course2keys}
    

import json
import httplib
import urllib

def getStudentList(filename):
    
    students = []
    f = open(filename)
    for lineCt, line in enumerate(f):
        line = line[:-1]
        if line.startswith('"') or not '\t' in line:
            print('The file should be tab-separated and not quoted.')
            return []
        fields = line.split('\t')
        email = fields[2]
        email = email.lower()
        precept = fields[3]
        if not precept.startswith('Precept'):
            precept = 'Precept ' + precept

        d = {'last' : fields[0],
             'first' : fields[1],
             'email' : email,
             'precept' : precept,
             }

        students.append(d)
            
    f.close()
    
    return students

def listDBStudents(connection, quiet=False):
    params = urllib.urlencode({"limit": 1000})
    
    connection.request('GET',
                       '/1/classes/StudentList?%s' % params, '',
                       {"X-Parse-Application-Id": keys[args.dbname][args.instance]['app'],
                        "X-Parse-Master-Key": keys[args.dbname][args.instance]['master'],})

    response = json.loads(connection.getresponse().read())
    results = response['results']
    print('Found %d students in database.' % len(results))

    results.sort(key=lambda x: (x['last'], x['first']))

    if not quiet:
        fieldNames = ['last', 'first', 'email', 'precept', 'objectId']
        for result in results:
            resultStr = ' '.join(['%-25s' % result[f] for f in fieldNames])
            print(resultStr)

    studentDBDict = dict([(s['email'], s) for s in results])

    return studentDBDict

def createDBSchema(connection):
    
    schema = {"className":"StudentList",
            "fields": {"last":{"type":"String"},
                        "first":{"type":"String"},
                        "email":{"type":"String"},
                        "precept":{"type":"String"},
                        }
                        }

    connection.request('POST',
                        '/1/schemas/StudentList',
                        json.dumps(schema),
                        {"X-Parse-Application-Id": keys[args.dbname][args.instance]['app'],
                         "X-Parse-Master-Key": keys[args.dbname][args.instance]['master'],
                        "Content-Type": "application/json"})

    result = json.loads(connection.getresponse().read())
    print(result)

def deleteStudents(connection, students):

    for student in students:
        connection.request('DELETE',
                           '/1/classes/StudentList/%s' % student['objectId'], '',
                           {"X-Parse-Application-Id": keys[args.dbname][args.instance]['app'],
                            "X-Parse-Master-Key": keys[args.dbname][args.instance]['master'],})
        response = json.loads(connection.getresponse().read())
        print(response)

def deleteAllStudents(connection):
    params = urllib.urlencode({"limit": 1000})

    connection.request('GET',
                       '/1/classes/StudentList?$%s' % params, '',
                       {"X-Parse-Application-Id": keys[args.dbname][args.instance]['app'],
                        "X-Parse-Master-Key": keys[args.dbname][args.instance]['master'],})

    response = json.loads(connection.getresponse().read())
    results = response['results']
    print('Found %d students in database.' % len(results))
    ans = raw_input('Delete ALL students from database -- are you sure? [y/N]: ')
    if ans != 'y':
        print('Nothing deleted.')
        return

    for result in results:
        connection.request('DELETE',
                           '/1/classes/StudentList/%s' % result['objectId'], '',
                           {"X-Parse-Application-Id": keys[args.dbname][args.instance]['app'],
                            "X-Parse-Master-Key": keys[args.dbname][args.instance]['master'],})
        response = json.loads(connection.getresponse().read())
        print(response)


def postStudents(connection, students):

    for student in students:
        js = json.dumps(student)
        print(js)
        connection.request('POST',
                           '/1/classes/StudentList',
                           json.dumps(student),
                           {"X-Parse-Application-Id": keys[args.dbname][args.instance]['app'],
                            "X-Parse-Master-Key": keys[args.dbname][args.instance]['master'],
                            "X-Parse-REST-API-Key": keys[args.dbname][args.instance]['api'],
                            "Content-Type": "application/json"})

        result = json.loads(connection.getresponse().read())
        print(result)

def updateStudents(connection, students):
    for student in students:
        
        params = urllib.urlencode({"where":json.dumps({"email": student['email']})})
        
        connection.request('GET',
                           '/1/classes/StudentList?%s' % params, '',
                           {"X-Parse-Application-Id": keys[args.dbname][args.instance]['app'],
                            "X-Parse-Master-Key": keys[args.dbname][args.instance]['master'],})

        response = json.loads(connection.getresponse().read())
        results = response['results']

        if len(results) == 0:
            print('Could not find student: %s' % (student['email']))
        elif len(results) > 1:
            print('Found %d students "%s"' % (len(results), student['email']))
        else:
            print('Found student %s: %s' % (student['email'], results[0]))

            data = json.dumps({"precept": student['precept']})
            connection.request('PUT',
                               '/1/classes/StudentList/%s' % results[0]['objectId'],
                               data,
                               {"X-Parse-Application-Id": keys[args.dbname][args.instance]['app'],
                                "X-Parse-Master-Key": keys[args.dbname][args.instance]['master'],
                                "X-Parse-REST-API-Key": keys[args.dbname][args.instance]['api'],
                                "Content-Type": "application/json"})

            result = json.loads(connection.getresponse().read())
            print(result)

def main(args):
    connection = httplib.HTTPSConnection('api.parse.com', 443)
    connection.connect()

    if args.list:
        listDBStudents(connection)
        return
        
    if args.create:
        createDBSchema(connection)
        return

    if args.deleteall:
        deleteAllStudents(connection)
        return
    
    students = getStudentList(args.filename)

    if not students:
        print('Did not find any students in %s, nothing done.' % args.filename)
        return
    
    print('Found %d students in file.' % len(students))

    if args.add or args.update or args.delete:
        studentFileDict = dict([(s['email'], s) for s in students])
        studentDBDict = listDBStudents(connection, quiet=True)

        newStudents = sorted(set(studentFileDict.keys()).difference(set(studentDBDict.keys())))
        oldStudents = sorted((set(studentDBDict.keys()).difference(set(studentFileDict.keys()))))
        sameStudents = sorted(set(studentFileDict.keys()).intersection(set(studentDBDict.keys())))

        newStudentList = [studentFileDict[s] for s in newStudents]
        oldStudentList = [studentDBDict[s] for s in oldStudents]
        sameStudentList = [studentFileDict[s] for s in sameStudents]
        
        fieldNames = ['last', 'first', 'email', 'precept']

        print('New students...')
        for s in newStudents:
            resultStr = ' '.join(['%-25s' % studentFileDict[s][f] for f in fieldNames])
            print(resultStr)

        print('Old students...')
        for s in oldStudents:
            resultStr = ' '.join(['%-25s' % studentDBDict[s][f] for f in fieldNames])
            print(resultStr)

        print('Students in both file and database...')
        for s in sameStudents:
            resultStr = ' '.join(['%-25s' % studentFileDict[s][f] for f in fieldNames])
            print(resultStr)

        print('Students with changed precept...')
        for s in sameStudents:
            if studentFileDict[s]['precept'] != studentDBDict[s]['precept']:
                print('Student: %s old precept: %s new precept: %s' % (s, studentDBDict[s]['precept'], studentFileDict[s]['precept']))
                
        msgList = []
        if args.add:
            msgList.append('add')
        if args.delete:
            msgList.append('delete')
        if args.update:
            msgList.append('update')

        ans = raw_input('Ok to %s students? [y/N]: ' % '/'.join(msgList))
        if ans != 'y':
            print('Nothing done.')
            return

        if args.add:
            postStudents(connection, newStudentList)

        if args.delete:
            deleteStudents(connection, oldStudentList)

        if args.update:
            updateStudents(connection, sameStudentList)

if __name__ == '__main__':

    import argparse

    parser = argparse.ArgumentParser(description='%s' % __doc__)

    parser.add_argument('-l', '--list', action='store_true', help='List the students and exit. (default: False)')
    parser.add_argument('-c', '--create', action='store_true', help='Create the StudentList schema and exit. (default: False)')
    parser.add_argument('--deleteall', action='store_true', help='Delete ALL the existing students from the database and exit. (default: False)')
    parser.add_argument('-f', '--filename', help='Tab-separated file of students.')
    parser.add_argument('-a', '--add', action='store_true', help='Add new students -- those in the file but not in the database. (default: False)')
    parser.add_argument('-d', '--delete', action='store_true', help='Delete students who are in the database but not in the file. (default: False)')
    parser.add_argument('-u', '--update', action='store_true', help='Using the email address as the identifier, update the database. (default: False)')
    parser.add_argument('-n', '--dbname', required=True, choices=['course1', 'course2'], help='Which database.')
    parser.add_argument('-i', '--instance', required=True, choices=['test', 'prod'], help='Which instance.')
 
    args = parser.parse_args()

    if (args.add or args.delete or args.update) and not args.filename:
        parser.error('--filename required with --add, --delete, or --update.')

    main(args)
    
