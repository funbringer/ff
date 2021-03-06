#ifndef ff_qt_TaskListWidget_hxx
#define ff_qt_TaskListWidget_hxx


#include <unordered_map>
#include <QtWidgets>

#include "FFI/Cxx.hxx"
#include "Model.hxx"


class TaskListWidget: public QTreeView {
    using super = QTreeView;
public:
    TaskListWidget(QWidget * parent, StorageHandle storage);
    void upsertTask(Note task);
    Model & model() const;
};


#endif // ff_qt_TaskListWidget_hxx
