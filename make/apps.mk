APP_FILES := $(wildcard ../lib/*/ebin/*.app)
APPS := $(basename $(notdir $(APP_FILES)))

apps: $(APPS:%=app_%)
app_%:
	cd $*; make app

clean: $(APPS:%=clean_%)
clean_%:
	cd $*; make clean
