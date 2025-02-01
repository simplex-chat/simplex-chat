import SwiftUI

struct TaskListView: View {
    @ObservedObject var taskModel: TaskModel

    var body: some View {
        NavigationView {
            List(taskModel.tasks) { task in
                NavigationLink(destination: TaskView(taskModel: taskModel, taskId: task.id)) {
                    VStack(alignment: .leading) {
                        Text(task.title)
                            .font(.headline)
                        Text(task.description)
                            .font(.subheadline)
                            .foregroundColor(.gray)
                    }
                }
            }
            .navigationTitle("Tasks")
        }
    }
}
