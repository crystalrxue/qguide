import selenium
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.common.keys import Keys
import time
import re
import sqlite3
import csv

def percent_extractor(bar):
    #bar = bar.get_attribute("alt")
    bar = bar.replace("%", "")
    for i in bar.split():
        if i.isdigit():
            return i

def name_reverser(name):
    i = name.find(",")
    if i == -1:
        return name
    return name[i+2:] + " " + name[:i]

#SEQUEL SETUP
connection = sqlite3.connect("info.db")
crsr = connection.cursor()

#SEQUEL SETUP

#CONSTANTS
FIRST_DEPARTMENT = 0
FIRST_CLASS = 0
#CONSTANTS

driver = webdriver.Chrome()
driver.maximize_window()

element = WebDriverWait(driver, 40).until(EC.presence_of_element_located((By.CLASS_NAME, "course-block"))) #Wait until we are on the first page of the Q guide.
time.sleep(5)

departments = driver.find_elements_by_class_name("course-block") #Get a list of all the deparmtent buttons.
for i in range(FIRST_DEPARTMENT, len(departments)): #Iterate over the department buttons.
    element = WebDriverWait(driver, 40).until(EC.presence_of_element_located((By.CLASS_NAME, "course-block"))) #Wait until we are on the homepage of the Q guide.
    departments = driver.find_elements_by_class_name("course-block") #Get a list of all the deparmtent buttons.
    department_title = departments[i].find_element_by_class_name("course-block-title") #Gets the title element of the current department button.
    department_title = department_title.text

    driver.execute_script("return arguments[0].scrollIntoView();", departments[i])

    click_failed = True
    clicks = 0
    #As many clicks as possible are contianed in a try-except statenment contained in a while loop with a counter.
    #If a click fails (e.g. b/c of connectivity issues), it is attempted 99 more times every 5 seconds. If it
    #still fails, then the program ends, provides the index of the class where it failed, and a manual
    #restart by the programmer is necessary. This has not happened so far.
    while(click_failed):
        departments[i].click()
        try:
            element = WebDriverWait(driver, 40).until(EC.presence_of_element_located((By.CLASS_NAME, "course")))
            click_failed = False
        except:
            departments = driver.find_elements_by_class_name("course-block")
        clicks += 1
        if clicks == 100:
            print("(i, j): " + str(i) + ", " + str(j))
            print("Website is taking too long to respond.")
            exit()


    classes = driver.find_elements_by_class_name("course")
    j = -1
    if i == FIRST_DEPARTMENT:
        j = FIRST_CLASS -1
    #Iterating over the classes in the ith department.
    while j < len(classes) - 1:
        j += 1
        print(i)
        print(j)
        classes = driver.find_elements_by_class_name("course")

        actions = ActionChains(driver)
        actions.move_to_element(classes[j]).perform()

        classes[j].click()
        try:
            element = WebDriverWait(driver, 40).until(EC.presence_of_element_located((By.ID, "reportContent")))
        except:
            driver.back()
            element = WebDriverWait(driver, 40).until(EC.presence_of_element_located((By.CLASS_NAME, "course-block")))
            departments = driver.find_elements_by_class_name("course-block")
            if j < len(classes)-1:
                driver.execute_script("return arguments[0].scrollIntoView();", departments[i])

                click_failed = True
                clicks = 0
                while(click_failed):
                    departments[i].click()
                    try:
                        element = WebDriverWait(driver, 40).until(EC.presence_of_element_located((By.CLASS_NAME, "course")))
                        click_failed = False
                    except:
                        departments = driver.find_elements_by_class_name("course-block")
                    clicks += 1
                    if clicks == 100:
                        print("(i, j): " + str(i) + ", " + str(j))
                        print("Website is taking too long to respond.")
                        exit()

            continue
        #Checks for missing data in the Q guide.
        try:
            title = driver.find_element_by_tag_name("h1").text
        except:
            title = "None"
        try:
            enrollment = re.findall("[0-9]+", (driver.find_element_by_id("summaryStats")).text)[0]
        except:
            enrollment = "None"
        try:
            rating = driver.find_element_by_xpath("/html/body/div[4]/div[2]/div[3]/div/table[1]/tbody/tr[2]/td[4]").text
        except:
            rating = "None"
        try:
            workload = driver.find_element_by_xpath("/html/body/div[4]/div[2]/div[3]/div/table[2]/tbody/tr[2]/td[4]").text
        except:
            workload = "None"
        try:
            recommendation = driver.find_element_by_xpath("/html/body/div[4]/div[2]/div[3]/div/table[3]/tbody/tr[2]/td[4]").text
        except:
            workload = "None"

        src = driver.page_source

        try:
            elective = re.findall("Choice question Elective is...", src)[0]
            elective = percent_extractor(elective)
        except:
            elective = "None"
        try:
            concentration = re.findall("Choice question Concentration or Department Requirement is...", src)[0]
            concentration = percent_extractor(concentration)
        except:
            concentration = "None"
        try:
            secondary_citation = re.findall("Choice question Secondary Field or Language Citation Requirement is...", src)[0]
            secondary_citation = percent_extractor(secondary_citation)
        except:
            secondary_citation = "None"
        try:
            core_gened = re.findall("Choice question Undergraduate Core or General Education Requirement is...", src)[0]
            core_gened = percent_extractor(core_gened)
        except:
            core_gened = "None"
        try:
            expos = re.findall("Choice question Expository Writing Requirement is...", src)[0]
            expos = percent_extractor(expos)
        except:
            expos = "None"
        try:
            language = re.findall("Choice question Foreign Language Requirement is...", src)[0]
            language = percent_extractor(language)
        except:
            language = "None"
        try:
            pre_med = re.findall("Choice question Pre-Med Requirement is...", src)[0]
            pre_med = percent_extractor(pre_med)
        except:
            pre_med = "None"

        #SEQUEL INSERT
        #The first chunk deals with classes.
        #md = """INSERT INTO classes (name, enrollment, overall, workload, department)
        #VALUES (?, ?, ?, ?, ?)"""
        #crsr.execute(cmd, (str(title), str(enrollment), str(rating), str(workload), str(department_title)))
        #connection.commit()
        #crsr.execute("SELECT * FROM classes WHERE name = ?", (str(title),))
        #ans=crsr.fetchall()
        #print(ans)
        with open('mycsv.csv', 'a', newline='') as f:
            thewriter = csv.writer(f)
            thewriter.writerow(["Spring 2018", department_title, str(title), str(workload), str(rating), str(enrollment), str(recommendation), str(elective), str(concentration), str(secondary_citation), str(core_gened), str(expos), str(language), str(pre_med)])
        #SEQUEL INSERT
        buttons = driver.find_element_by_id("tabNav")
        buttons = driver.find_elements_by_tag_name("li")

        #The second chunk deals with professors.

        buttons = driver.find_element_by_id("tabNav")
        buttons = driver.find_elements_by_tag_name("li")

        #Returns to the homepage.
        buttons = driver.find_element_by_class_name("helpLink")
        buttons = buttons.find_element_by_tag_name("a")

        click_failed = True
        clicks = 0
        while(click_failed):
            buttons.click()
            try:
                element = WebDriverWait(driver, 40).until(EC.presence_of_element_located((By.CLASS_NAME, "course-block")))
                click_failed = False
            except:
                buttons = driver.find_element_by_class_name("helpLink")
                buttons = buttons.find_element_by_tag_name("a")
            clicks += 1
            if clicks == 100:
                print("(i, j): " + str(i) + ", " + str(j))
                print("Website is taking too long to respond.")
                exit()


        element = WebDriverWait(driver, 40).until(EC.presence_of_element_located((By.CLASS_NAME, "course-block")))
        departments = driver.find_elements_by_class_name("course-block")
        if j < len(classes)-1:
            driver.execute_script("return arguments[0].scrollIntoView();", departments[i])

            click_failed = True
            clicks = 0
            while(click_failed):
                departments[i].click()
                try:
                    element = WebDriverWait(driver, 40).until(EC.presence_of_element_located((By.CLASS_NAME, "course")))
                    click_failed = False
                except:
                    departments = driver.find_elements_by_class_name("course-block")
                clicks += 1
                if clicks == 100:
                    print("(i, j): " + str(i) + ", " + str(j))
                    print("Website is taking too long to respond.")
                    exit()
