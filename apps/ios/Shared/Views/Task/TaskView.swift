import SwiftUI

struct TaskView: View {
    @ObservedObject var taskModel: TaskModel
    var taskId: UUID

    var task: Task? {
        taskModel.tasks.first { $0.id == taskId }
    }

    var body: some View {
        VStack(alignment: .leading) {
            if let task = task {
                Text(task.title)
                    .font(.title)
                    .padding(.bottom, 5)

                Text(task.description)
                    .font(.body)
                    .padding(.bottom, 5)

                Text("Due Date: \(task.dueDate, formatter: dateFormatter)")
                    .font(.subheadline)
                    .padding(.bottom, 5)

                Text("Completed: \(task.isCompleted ? "Yes" : "No")")
                    .font(.subheadline)
                    .padding(.bottom, 5)

                Text("User ID: \(task.userId.uuidString)")
                    .font(.subheadline)
                    .padding(.bottom, 5)
            } else {
                Text("Task not found")
                    .font(.title)
                    .foregroundColor(.red)
            }
        }
        .padding()
    }

    private var dateFormatter: DateFormatter {
        let formatter = DateFormatter()
        formatter.dateStyle = .medium
        return formatter
    }
}
